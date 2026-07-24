{-# OPTIONS_GHC -cpp -XNondecreasingIndentation #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager                                                                  ------
----------------------------------------------------------------------------------------------------
module FileManager where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Numeric hiding (readInt)
import System.IO.Unsafe
import System.Cmd
import System.Process
#if defined(FREEARC_WIN)
import System.Win32
import Foreign.Ptr
#endif

import Graphics.UI.Gtk hiding (on)
import Graphics.UI.Gtk.ModelView as New
import qualified System.Glib.Signals as GtkSigs
import Graphics.UI.Gtk.Abstract.Widget (keyPressEvent, buttonPressEvent, configureEvent, windowStateEvent)
import Graphics.UI.Gtk.ActionMenuToolbar.Action (actionActivated)
import Graphics.UI.Gtk.Gdk.EventM (eventCoordinates, eventButton, eventClick, eventWindowState, eventKeyName)
import Graphics.UI.Gtk.Entry.Entry (entryActivated)
import Graphics.UI.Gtk.Abstract.Object (objectDestroy)
import qualified Data.Text as T

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Compression
import Encryption
import Options
import Cmdline
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import FileManPanel
import FileManUtils
import FileManDialogs
import FileManDialogAdd

-- |Maximum size of command line
aMAX_CMDLINE_LENGTH = 32000 `div` 4

----------------------------------------------------------------------------------------------------
---- ��������� GUI-����������� �������� ��������� ������ -------------------------------------------
----------------------------------------------------------------------------------------------------

parseGUIcommands run args exec = do
  let extract fm' exec cmd arcnames = extractDialog fm' exec cmd arcnames "" []
      add     fm' exec cmd files    = addDialog     fm' exec cmd files NoMode
  loadTranslation
  case args of
    ["--settings-dialog"] -> openSettingsDialog               -- ������ ��������
    "--add-dialog":xs     -> openDialog xs exec add           -- ������ ��������
    "--extract-dialog":xs -> openDialog xs exec extract       -- ������ ����������
    ["--register"]        -> registerShellExtensions Nothing  -- ����������� � Explorer
    ["--unregister"]      -> unregisterShellExtensions        -- �������� ����������� � Explorer
    []                    -> myGUI run args                   -- ��� ������ ��������� ��� ���������� ��� � ����� ���������� (������ ��������/������)
    [_]                   -> myGUI run args                   --   ��������� ����������� Archive Manager
    _                     -> startGUI >> exec args            --   � ����� - ������ ������������ ������� (��)���������

-- ������ ��������
openSettingsDialog = do
  startGUI
  gui $ do
    fm' <- newEmptyFM
    settingsDialog fm'
    mainQuit

-- ������� ������ (���)������� � ����� ��������� ����������� �������
openDialog (cmd:"--":params) exec dialog = do
  startGUI
  cmdChan <- newChan
  gui $ do
    let exec _bgmode = writeChan cmdChan
    fm' <- newEmptyFM
    dialog fm' exec cmd params
  --
  cmds <- readChan cmdChan
  exec$ joinWith [";"] cmds

openDialog params exec dialog = do
  startGUI
  gui $ debugMsg "FileManager.hs: attempt to run dialog with incorrect parameters to command"


----------------------------------------------------------------------------------------------------
---- ������� ���� ��������� � ������ ��� ��� -------------------------------------------------------
----------------------------------------------------------------------------------------------------
--      File: New Archive, Open Archive, New SFX, Change Drive, Select All, Select Group, Deselect Group, Invert Selection
--      Commands (��� Actions): Add, Extract, Test, ArcInfo, View, Delete, Rename
--      Tools: Wizard (���� ������� �����), Protect, Comment, Convert to EXE, Encrypt, Add Recovery record, Repair
--      Options: Configuration, Save settings, Load settings, View log, Clear log
--      Help: ���������� ��� Help, Goto Homepage (�/��� Check for update), About

uiDef =
  "<ui>"++
  "  <menubar>"++
  "    <menu name=\"File\"     action=\"FileAction\">"++
  "      <menuitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Select all\"         action=\"SelectAllAction\" />"++
  "      <menuitem name=\"Select\"             action=\"SelectAction\" />"++
  "      <menuitem name=\"Unselect\"           action=\"UnselectAction\" />"++
  "      <menuitem name=\"Invert selection\"   action=\"InvertSelectionAction\" />"++
  "      <menuitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "      <separator/>"++
  "      <placeholder name=\"FileMenuAdditions\" />"++
  "      <menuitem name=\"Exit\"               action=\"ExitAction\"/>"++
  "    </menu>"++
  "    <menu name=\"Commands\" action=\"CommandsAction\">"++
  "      <menuitem name=\"Add\"                action=\"AddAction\" />"++
  "      <menuitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <menuitem name=\"Test\"               action=\"TestAction\" />"++
  "      <menuitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <menuitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "    </menu>"++
  "    <menu name=\"Tools\"    action=\"ToolsAction\">"++
  "      <menuitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <menuitem name=\"Comment\"            action=\"CommentAction\" />"++
  "      <menuitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <menuitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <menuitem name=\"Convert to FreeArc\" action=\"ConvertToFreeArcAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Encrypt\"            action=\"EncryptAction\" />"++
  "      <menuitem name=\"Protect\"            action=\"ProtectAction\" />"++
  "      <menuitem name=\"Repair\"             action=\"RepairAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Modify\"             action=\"ModifyAction\" />"++
  "      <menuitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "    </menu>"++
  "    <menu name=\"Options\"  action=\"OptionsAction\">"++
  "      <menuitem name=\"Settings\"           action=\"SettingsAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"ViewLog\"            action=\"ViewLogAction\" />"++
  "      <menuitem name=\"ClearLog\"           action=\"ClearLogAction\" />"++
  "    </menu>"++
  "    <menu name=\"Help\"     action=\"HelpAction\">"++
  "      <menuitem name=\"MainHelp\"           action=\"MainHelpAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"CmdlineHelp\"        action=\"CmdlineHelpAction\" />"++
  "      <menuitem name=\"OpenHomepage\"       action=\"OpenHomepageAction\" />"++
  "      <menuitem name=\"OpenForum\"          action=\"OpenForumAction\" />"++
  "      <menuitem name=\"CheckForUpdate\"     action=\"CheckForUpdateAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"About\"              action=\"AboutAction\" />"++
  "    </menu>"++
  "  </menubar>"++
  "  <toolbar>"++
  "    <placeholder name=\"FileToolItems\">"++
  "      <toolitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Add\"                action=\"AddAction\" />"++
  "      <toolitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <toolitem name=\"Test\"               action=\"TestAction\" />"++
  "      <toolitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <toolitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <toolitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <toolitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <toolitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "    </placeholder>"++
  "  </toolbar>"++
  "</ui>"


----------------------------------------------------------------------------------------------------
---- ���������� ����� ����-��������� ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

myGUI run args = do
  fileManagerMode =: True
  runGUI $ do
  --parseCmdline ["l", "a"]   -- Removed: causes 10s+ delay in GUI mode due to missing config files
  -- ������ ���������� �������->��������
  onKeyActions <- newList
  let onKey = curry (onKeyActions <<=)
  -- �������� ���� ���������� ��������� � �������� ���������/�����������
  (windowProgress, (clearMessageBox,showMessageBox)) <- runIndicators
  -- Main menu
  standardGroup <- actionGroupNew "standard"
  let action name  =  (concat$ map (mapHead toUpper)$ words$ drop 5 name)++"Action"   -- "9999 the name" -> "TheNameAction"
  let names = split ',' "0050 File,0258 Commands,0259 Tools,0260 Options,0261 Help"
  for names $ \name -> do
    label <- i18n name
    actionGroupAddAction standardGroup  =<<  actionNew (action name) label Nothing Nothing
  -- Menus and toolbars
  let anew name comment icon accel = do
        [i18name,i18comment] <- i18ns [name,comment]
        action <- actionNew (action name) i18comment (Just i18comment) icon
        action `set` [actionShortLabel := i18name]
        actionGroupAddActionWithAccel standardGroup action (Just accel)
        accel `onKey` actionActivate action
        return action
  --
  openAct     <- anew "0262 Open archive"        "0265 Open archive"                              (Just stockOpen)            "<Alt>O"
  selectAllAct<- anew "0263 Select all"          "0290 Select all files"                          (Just stockSelectAll)       "<Ctrl>A"
  selectAct   <- anew "0037 Select"              "0047 Select files"                              (Just stockAdd)             "KP_Add"
  unselectAct <- anew "0038 Unselect"            "0048 Unselect files"                            (Just stockRemove)          "KP_Subtract"
  invertSelAct<- anew "0264 Invert selection"    "0291 Invert selection"                          (Nothing)                   "KP_Multiply"
  refreshAct  <- anew "0039 Refresh"             "0049 Reread archive/directory"                  (Just stockRefresh)         "F5"
  exitAct     <- anew "0036 Exit"                "0046 Quit application"                          (Just stockQuit)            "<Alt>Q"

  addAct      <- anew "0030 Add"                 "0040 Add files to archive(s)"                   (Just stockMediaRecord)     "<Alt>A"
  extractAct  <- anew "0035 Extract"             "0045 Extract files from archive(s)"             (Just stockMediaPlay)       "<Alt>E"
  testAct     <- anew "0034 Test"                "0044 Test files in archive(s)"                  (Just stockSpellCheck)      "<Alt>T"
  arcinfoAct  <- anew "0086 ArcInfo"             "0087 Information about archive"                 (Just stockInfo)            "<Alt>I"
  deleteAct   <- anew "0033 Delete"              "0043 Delete files (from archive)"               (Just stockDelete)          "Delete"

  lockAct     <- anew "0266 Lock"                "0267 Lock archive from further changes"         (Just stockDialogAuthentication) "<Alt>L"
  commentAct  <- anew "0268 Comment"             "0269 Edit archive comment"                      (Just stockEdit)            "<Alt>C"
  recompressAct<-anew "0293 Recompress"          "0294 Recompress files in archive"               (Just stockGotoBottom)      "<Alt>R"
  toSfxAct    <- anew "0270 Convert to SFX"      "0271 Convert archive to SFX"                    (Just stockConvert)         "<Alt>S"
  toFaAct     <- anew "0426 Convert to FreeArc"  "0427 Convert foreign archive to FreeArc format" (Nothing)                   ""
  encryptAct  <- anew "0272 Encrypt"             "0273 Encrypt archive contents"                  (Nothing)                   ""
  addRrAct    <- anew "0274 Protect"             "0275 Add Recovery record to archive"            (Nothing)                   "<Alt>P"
  repairAct   <- anew "0379 Repair"              "0380 Repair damaged archive"                    (Nothing)                   ""
  modifyAct   <- anew "0031 Modify"              "0041 Modify archive(s)"                         (Just stockEdit)            "<Alt>M"
  joinAct     <- anew "0032 Join archives"       "0042 Join archives together"                    (Just stockCopy)            "<Alt>J"

  settingsAct <- anew "0064 Settings"            "0065 Edit program settings"                     (Just stockPreferences)     ""
  viewLogAct  <- anew "0276 View log"            "0277 View logfile"                              (Nothing)                   ""
  clearLogAct <- anew "0278 Clear log"           "0279 Clear logfile"                             (Nothing)                   ""

  helpAct     <- anew "0280 Main help"           "0281 Help on using FreeArc"                     (Just stockHelp)            "F1"
  helpCmdAct  <- anew "0282 Cmdline help"        "0283 Help on FreeArc command line"              (Just stockHelp)            ""
  homepageAct <- anew "0284 Open Homepage"       "0285 Open program site"                         (Just stockHome)            ""
  openForumAct<- anew "0373 Open forum"          "0374 Open program forum"                        (Nothing)                   ""
  openWikiAct <- anew "0375 Open wiki"           "0376 Open program wiki"                         (Nothing)                   ""
  whatsnewAct <- anew "0286 Check for update"    "0287 Check for new program version"             (Just stockDialogInfo)      ""
  aboutAct    <- anew "0288 About"               "0289 About"                                     (Just stockAbout)           ""

  menufile <- findFile configFilePlaces aMENU_FILE
  uiData   <- if menufile>""  then fileGetBinary menufile  else return uiDef

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiData
  uiManagerInsertActionGroup ui standardGroup 0

  window <- windowNew
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

  (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <- createFilePanel
  statusLabel  <- labelNew (Nothing :: Maybe String)
  miscSetAlignment statusLabel 0 0.5
  messageCombo <- New.comboBoxNewText
  statusbar    <- statusbarNew
  ctx <- statusbarGetContextId statusbar ""
  statusbarPush statusbar ctx "    "
  widgetSetSizeRequest messageCombo 30 (-1)
  lowBox <- vBoxNew False 0
  boxPackStart lowBox statusLabel  PackNatural 2
  boxPackStart lowBox messageCombo PackGrow    2
  --boxPackStart lowBox statusbar    PackNatural 0

  -- �������� ���������� ��� �������� �������� ��������� ����-���������
  fm' <- newFM window listView listModel listSelection statusLabel messageCombo
  fmUpdateConfigFiles fm'

  -- ��������� ������
  let toolbar = castToToolbar toolBar
  toolbarCaptions <- fmGetHistoryBool fm' "ToolbarCaptions" True
  toolbar `set` [toolbarStyle := if toolbarCaptions then ToolbarBoth else ToolbarIcons]
  -- toolbarSetIconSize toolbar IconSizeLargeToolbar
  n <- toolbarGetNItems toolbar
  for [0..n-1] $ \i -> do
    Just button <- toolbarGetNthItem toolbar i
    toolItemSetHomogeneous button False

  -- ������� ���������
  naviBar  <- hBoxNew False 0
  upButton <- button "0006   Up  "
  curdir   <- fmEntryWithHistory fm' "dir/arcname" (const$ return True) (fmCanonicalizePath fm')
  saveDirButton <- button "0007   Save  "
  boxPackStart naviBar (widget upButton)       PackNatural 0
#if defined(FREEARC_WIN)
  -- ���� ������ �����
  driveButton <- button "C:"
  driveMenu   <- makePopupMenu (chdir fm'.(++"\\").head.words) =<< getDrives
  driveButton `onClick` (widgetShowAll driveMenu >> menuPopup driveMenu Nothing)
  boxPackStart naviBar (widget driveButton)    PackNatural 0
#endif
  boxPackStart naviBar (widget curdir)         PackGrow    0
  boxPackStart naviBar (widget saveDirButton)  PackNatural 0

  -- ������� ���� ����-���������
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar   PackNatural 0
  boxPackStart vBox toolBar   PackNatural 0
  boxPackStart vBox naviBar   PackNatural 0
  boxPackStart vBox listUI    PackGrow    0
  boxPackStart vBox lowBox    PackNatural 0

  containerAdd window vBox


  -- ������ ��������, ����������� ��� �������� ���� ����-���������
  onExit <- newList

  -- ������ ���������� �������->��������
  GtkSigs.on listView keyPressEvent $ do
    name <- eventKeyName
    x <- liftIO $ lookup (T.unpack name) `fmap` listVal onKeyActions
    case x of
      Just action -> do liftIO action; return True
      Nothing     -> return False


----------------------------------------------------------------------------------------------------
---- ����������/�������������� ������� � ��������� �������� ���� � ������� � ��� -------------------
----------------------------------------------------------------------------------------------------

  window `windowSetPosition` WinPosCenter
  --windowSetGeometryHints window (Just window) (Just (1,1)) (Just (32000,32000)) Nothing Nothing Nothing
  --widgetSetSizeRequest window 700 500
  --window `windowSetGravity` GravityStatic
  --window `windowSetPosition` WinPosNone
  --windowSetDefaultSize window 200 100

  -- ��� ������ ����������� ����������� ������ ����
  fmRestoreSizePos fm' window "MainWindow" "-10000 -10000 720 500"

  -- �������� ������ � ��������� �������� ���� ����� ��� �����������
  GtkSigs.on window configureEvent $ do
    liftIO $ fmSaveSizePos fm' window "MainWindow"
    return False

  -- ��������, ���� �� ���� ���������������
  GtkSigs.on window windowStateEvent $ do
    let isMax x = case x of
                    WindowStateMaximized -> True
                    _                    -> False
    ws <- eventWindowState
    liftIO $ fmSaveMaximized fm' "MainWindow" (any isMax ws)
    return False


  -- ��� �������� ��������� �������� ������� � ������ �������
  onExit <<= do
    colnames  <-  New.treeViewGetColumns listView  >>=  mapM New.treeViewColumnGetTitle
    fmReplaceHistory fm' "ColumnOrder" (unwords$ catMaybes colnames)
    for columns $ \(name,col1) -> do
      w <- New.treeViewColumnGetWidth col1
      fmReplaceHistory fm' (name++"ColumnWidth") (show w)

  -- ��� ������ ����������� ����������� ������� � ������ �������
  order <- (reverse.words) `fmap` fmGetHistory1 fm' "ColumnOrder" ""
  for order $ \colname -> do
    whenJust (lookup colname columns) $
      New.treeViewMoveColumnFirst listView
  for columns $ \(name,col1) -> do
    w <- readInt  `fmap`  fmGetHistory1 fm' (name++"ColumnWidth") "150"
    New.treeViewColumnSetFixedWidth col1 w


----------------------------------------------------------------------------------------------------
---- ������������� ����� ����-��������� ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--  for [upButton,saveDirButton] (`buttonSetFocusOnClick` False)

  -- �������� errors/warnings/messages ����� ���� FreeArc
  showErrors' <- ref True
  errorHandlers   ++= [whenM (val showErrors') . condPrintLineLn "w"]
  errorHandlers   ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  warningHandlers ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  loggingHandlers ++= [postGUIAsync . fmStackMsg fm']

  -- ��������� ����� ��������� �� ������� �� ����� ���������� action
  let hideErrors action  =  bracket (showErrors' <=> False)  (showErrors' =: )  (\_ -> action)

  -- �������� � ��������� ������
  errorMsg <- ref ""
  errorHandlers ++= [(errorMsg =:)]
  let withErrorHandler onError = handle (\(e :: SomeException) -> do
        operationTerminated =: False
        fmErrorMsg fm' =<< val errorMsg
        sequence_ onError)
  -- ��� ������������� ������ ������ � ������������
  let msgboxOnError = withErrorHandler []
  -- ��� ������������� ������ ������ � ������������ � ��������� ���������� ���������
  let terminateOnError = withErrorHandler [shutdown "" aEXIT_CODE_FATAL_ERROR]


  -- ������� � �������� �������/����� ��� ��������� �������
  let select filename = do
        fm <- val fm'
        handle (\(_ :: SomeException) -> (operationTerminated =: False) >> runFile filename (fm_curdir fm) False) $ do    -- ��� ������� �������� �������� ���� :)
          hideErrors $ do
            chdir fm' filename
            New.treeViewScrollToPoint (fm_view fm) 0 0
            --New.treeViewSetCursor (fm_view fm) [0] Nothing

  -- ������� � ������������ �������
  let goParentDir = do
        fm <- val fm'
        unless (isFM_Archive fm  &&  isURL(fm_arcname fm)  &&  fm_arcdir fm=="") $ do  -- ��������� Up �� ������ � �����
        chdir fm' ".."
        -- �������� �������/�����, �� �������� �� ������ ��� �����
        fmSetCursor fm' (takeFileName$ fm_current fm)

  -- ������ �������� �������� � �������
  let saveCurdirToHistory = do
        fm <- val fm'
        fmAddHistory fm' (isFM_Archive fm.$bool "dir" "arcname") =<< fmCanonicalizePath fm' =<< val curdir


  -- ��� ������� Enter �� ������ � ������ ��������� ��������� �����/�������
  GtkSigs.on listView New.rowActivated $ \path column -> do
    fm <- val fm'
    file <- fmFileAt fm' path
    unless (isFM_Archive fm  &&  not(fdIsDir file)) $ do  -- Run command don't yet work directly from archives
    select (fmname file)

  -- ��� single-click �� ��������� ������������ ������/����� ������� ������� �� ���� ������,
  -- ��� double-click ��� �� �������� ��� �����
  GtkSigs.on listView buttonPressEvent $ do
    (ex, ey) <- eventCoordinates
    path <- liftIO $ New.treeViewGetPathAtPos listView (round ex, round ey)
    coltitle <- case path of
                  Just (_,column,_) -> liftIO $ New.treeViewColumnGetTitle column >>== fromMaybe ""
                  _                 -> return ""
    btn <- eventButton
    clk <- eventClick
    liftIO $ coltitle=="" &&& btn==LeftButton &&&
      ((if clk==SingleClick  then fmUnselectAll  else fmSelectAll) fm'  >>  return True)

  -- ��� �������� � ������ �������/����� ���������� ��� ��� � ������ �����
  fm' `fmOnChdir` do
    fm <- val fm'
    curdir =: fm_current fm
    -- ��������� � ������� ����� �������
    isFM_Archive fm  &&&  fm_arcdir fm==""  &&&  saveCurdirToHistory
    -- ���������� ������� � �����
    rereadHistory curdir

  -- ��� �������� � ������ �������/����� - ���������� ��� ��� � ��������� ����
  fm' `fmOnChdir` do
    fm <- val fm'
    let title | isFM_Archive fm  =  takeFileName (fm_arcname fm) </> fm_arcdir fm
              | otherwise        =  takeFileName (fm_dir fm)  |||  fm_dir fm
    set (fm_window fm) [windowTitle := title++" - "++aARC_NAME]

  -- ��������� � ���. ������� �� ������ Up ��� ������� BackSpace � ������ ������
  upButton  `onClick` goParentDir
  "BackSpace" `onKey` goParentDir

  -- ���������� ���������� ������/�������� � �������
  saveDirButton `onClick` do
    saveCurdirToHistory

  -- �������� ������� �������� ��� ������ (Enter � ������ �����)
  GtkSigs.on (entry curdir) entryActivated $ do
    saveCurdirToHistory
    select =<< val curdir

  -- �������� ������� �������� ��� ������ (����� �� �������)
  GtkSigs.on (entry curdir) entryActivated $ do
    saveCurdirToHistory
    select =<< val curdir

  -- ��������� action ��� �������, ���������� � ����������� makeRE � ������ ����� ��� ��������
  let byFile action makeRE = do
        filename <- fmGetCursor fm'
        action fm' ((match$ makeRE filename).fdBasename)

  -- ������� Shift/Ctrl/Alt-Plus/Minus � ���� �� ���������� ��� � FAR
  "<Shift>KP_Add"      `onKey` fmSelectAll   fm'
  "<Shift>KP_Subtract" `onKey` fmUnselectAll fm'
  "<Ctrl>KP_Add"       `onKey` byFile fmSelectFilenames   (("*" ++).takeExtension)
  "<Ctrl>KP_Subtract"  `onKey` byFile fmUnselectFilenames (("*" ++).takeExtension)
  "<Alt>KP_Add"        `onKey` byFile fmSelectFilenames   ((++".*").dropExtension)
  "<Alt>KP_Subtract"   `onKey` byFile fmUnselectFilenames ((++".*").dropExtension)


  -- ��� ������� ��������� ������� � ������ ������ - ����������� �� ����� �������
  --   (��� ��������� ������� - ����������� � �������� �������)
  onColumnTitleClicked =: \column -> do
    fmModifySortOrder fm' (showSortOrder columns) (calcNewSortOrder column)
    refreshCommand fm'
    fmSaveSortOrder  fm' =<< fmGetSortOrder fm'  -- ������� � ������ ������� ����������

  -- ����������� ����� �� ������������ ��������
  fmSetSortOrder fm' (showSortOrder columns) =<< fmRestoreSortOrder fm'


----------------------------------------------------------------------------------------------------
---- ������ ���������� ���������� ������ ������ FM gui ---------------------------------------------
----------------------------------------------------------------------------------------------------

  -- ��� ���������� �������� �� ������� �� �����������, � �������� ��������� � ��� � �������
  let myHandleErrors action  =  do operationTerminated =: False
                                   parent_id =:: myThreadId
                                   action `catch` handler
                                   whenM (val operationTerminated) $ do
                                     sleepSeconds 0.1
                                     operationTerminated =: False
        where bg action = do
                done <- newEmptyMVar
                forkIO (action >> putMVar done ())
                forkIO (foreverM (sleepSeconds 0.01 >> whenM (val operationTerminated) (putMVar done ())))
                takeMVar done

              handler (ex :: SomeException) = do
                unlessM (val operationTerminated) $ do
                  let errmsg = displayException ex
                  condPrintLineLn "w" errmsg
                  return ()
                condPrintLineLn "w" ""

  -- ����, ����������� ������� ����������
  cmdChan <- newChan
  forkIO $ do
    foreverM $ do
      commands <- readChan cmdChan
      when (commands==[["ExitProgram"]])  $ do fileManagerMode =: True; shutdown "" aEXIT_CODE_SUCCESS
      postGUIAsync$ do widgetShowAll windowProgress
      for commands $ \cmd -> do
        myHandleErrors (parseCmdline cmd >>= mapM_ run)
      postGUIAsync$ do widgetHide windowProgress; clearMessageBox; warningsBefore =:: val warnings; refreshCommand fm'
      --uiDoneProgram

  -- Depending on execution mode, either queue commands or run external FreeArc instances
  let exec detach cmds =
        if detach
          then do freearc <- getExeName
                  fm <- val fm'
                  for cmds $ \cmd -> do
                    let full_cmd = unparseCommand (freearc:cmd)
                    if length(full_cmd) < aMAX_CMDLINE_LENGTH
                      then Files.runCommand full_cmd (fm_curdir fm) False
                      else forkIO_ $ do
                             withTempFile (unicode2utf8$ unparseCommand cmd) $ \cmdfile -> do
                               Files.runCommand (unparseCommand [freearc,'@':cmdfile]) (fm_curdir fm) True

          else writeChan cmdChan cmds

  -- �������� ���� ����-���������
  let closeMainWindow = do
        sequence_ =<< listVal onExit
        fileManagerMode =: False
        showMessageBox
        widgetHide window
        writeChan cmdChan [["ExitProgram"]]

  GtkSigs.on window objectDestroy $ closeMainWindow


----------------------------------------------------------------------------------------------------
---- ���� File -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- ������� �����
  GtkSigs.on openAct actionActivated $ do
    fm <- val fm'
    let curfile  =  if isFM_Archive fm  then fm_arcname fm  else fm_dir fm </> "."
    chooseFile window FileChooserActionOpen "0305 Open archive" aARCFILE_FILTER (return curfile) $ \filename -> do
      msgboxOnError $
        chdir fm' filename

  -- Select/unselect files by user-supplied mask
  let byDialog method msg = do
        whenJustM_ (fmInputString fm' "mask" msg (const$ return True) return) $ \mask -> do
          method fm' ((match mask).fdBasename)
  GtkSigs.on selectAct actionActivated $  byDialog fmSelectFilenames   "0008 Select files"
  GtkSigs.on unselectAct actionActivated $  byDialog fmUnselectFilenames "0009 Unselect files"

  -- �������� ��� �����
  GtkSigs.on selectAllAct actionActivated $ do
    fmSelectAll fm'

  -- ������������� ���������
  GtkSigs.on invertSelAct actionActivated $ do
    fmInvertSelection fm'

  -- �������� ������ ������ ����������� �������
  GtkSigs.on refreshAct actionActivated $ do
    refreshCommand fm'

  -- ����� �� ���������
  GtkSigs.on exitAct actionActivated
    closeMainWindow


----------------------------------------------------------------------------------------------------
---- ���� Commands ---------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- �������� ������
  GtkSigs.on addAct actionActivated $ do
    compressionOperation fm' addDialog exec "a" NoMode

  -- ���������� �����(��)
  GtkSigs.on extractAct actionActivated $ do
    archiveOperation fm' $
      extractDialog fm' exec "x"
    rereadHistory curdir

  -- ������������ �����(��)
  GtkSigs.on testAct actionActivated $ do
    archiveOperation fm' $
      extractDialog fm' exec "t"

  -- ���������� �� ������
  GtkSigs.on arcinfoAct actionActivated $ do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec NoMode

  -- �������� ������ (�� ������)
  GtkSigs.on deleteAct actionActivated $ do
    fm <- val fm'
    files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
    if null files  then fmErrorMsg fm' "0012 There are no files selected!" else do
    msg <- i18n$ case files of [_] | isFM_Archive fm -> "0160 Delete %1 from archive?"
                                   | otherwise       -> "0161 Delete %1?"
                               _   | isFM_Archive fm -> "0019 Delete %2 file(s) from archive?"
                                   | otherwise       -> "0020 Delete %2 file(s)?"
    whenM (askOkCancel window (formatn msg [head files, show3$ length files])) $ do
      fmDeleteSelected fm'
      if isFM_Archive fm
        -- ������� ����� �� ������
        then do closeFMArc fm'
                let arcname = fm_arcname fm
                exec False [["d", "--noarcext", "--", arcname]++files]
        -- ������� ����� �� �����
        else mapM_ (ignoreErrors.fileRemove.(fm_dir fm </>)) files


----------------------------------------------------------------------------------------------------
---- ���� Tools ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- �������� ����� �� ������
  GtkSigs.on lockAct actionActivated $ do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0299 Lock archive(s)?"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec False [["ch", "--noarcext", "-k", "--", arcname]]

  -- �������� ����������� ������
  GtkSigs.on commentAct actionActivated $ do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec CommentMode

  -- ������������� ����� � SFX
  GtkSigs.on recompressAct actionActivated $ do
    compressionOperation fm' addDialog exec "ch" RecompressMode

  -- ������������� ����� � SFX
  GtkSigs.on toSfxAct actionActivated $ do
    compressionOperation fm' addDialog exec "ch" MakeSFXMode

  -- ������������� ����� ����� � ������ FreeArc
  GtkSigs.on toFaAct actionActivated $ do
    compressionOperation fm' addDialog exec "cvt" NoMode

  -- ����������� �����
  GtkSigs.on encryptAct actionActivated $ do
    compressionOperation fm' addDialog exec "ch" EncryptionMode

  -- �������� RR � �����
  GtkSigs.on addRrAct actionActivated $ do
    compressionOperation fm' addDialog exec "ch" ProtectionMode

  -- ������������ ����������� �����
  GtkSigs.on repairAct actionActivated $ do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0381 Repair archive(s)? Repaired archive(s) will be placed into files named fixed.*"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec False [["r", "--noarcext", "--", arcname]]

  -- ����������� �������
  GtkSigs.on modifyAct actionActivated $ do
    compressionOperation fm' addDialog exec "ch" NoMode

  -- ����������� �������
  GtkSigs.on joinAct actionActivated $ do
    compressionOperation fm' addDialog exec "j" NoMode


----------------------------------------------------------------------------------------------------
---- ���� Options ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- ���� �������� ���������
  GtkSigs.on settingsAct actionActivated $ do
    settingsDialog fm'

  -- �������� � ���������
  let withLogfile action = do
        logfileHist <- fmGetHistory fm' "logfile"
        case logfileHist of
          logfile:_ | logfile>""  ->  action logfile
          _                       ->  fmErrorMsg fm' "0303 No log file specified in Settings dialog!"

  -- ����������� �������
  GtkSigs.on viewLogAct actionActivated $ do
    withLogfile runViewCommand

  -- ������� �������
  GtkSigs.on clearLogAct actionActivated $ do
    withLogfile $ \logfile -> do
      msg <- i18n"0304 Clear logfile %1?"
      whenM (askOkCancel window (format msg logfile)) $ do
        filePutBinary logfile ""


----------------------------------------------------------------------------------------------------
---- ���� Help -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Home/news page for the current locale
  homeURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0254 "
  newsURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0255 News.aspx"
  forumURL <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0457 redirects/forum.aspx"
  wikiURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0458 redirects/wiki.aspx"

  -- ������� URL
#ifdef FREEARC_WIN
  let openWebsite url  =  runFile url "." False
#else
  let openWebsite url  =  do browser <- runProgram "gconftool-2 --get '/desktop/gnome/url-handlers/http/command'"
                             browser <- return$ if "%s" `isInfixOf` browser  then browser  else "firefox \"%s\""
                             System.Process.runCommand (browser.$ replaceAll "%s" url)
                             return ()
#endif

  -- ������� ���� ������
  let openHelp helpfile = do
        doc  <- i18n helpfile
        file <- findFile libraryFilePlaces (iif isWindows "../Documentation" "Documentation" </> doc)
        case file of
          "" -> return ()
          _  -> openWebsite ((isWindows&&&windosifyPath) file)

  -- ��������� ��, ���������� ��� ����� ����������
  let getUserID = do
#ifndef FREEARC_WIN
        -- �������� ������������� Windows Registry ��� ������ ��
        let registryGetStr root branch key       = return Nothing
            registrySetStr root branch key value = return ()
            hKEY_LOCAL_MACHINE                   = ()
#endif
        -- ������� ���� ��� � ���-�����
        userid <- fmGetHistory1 fm' "UserID" ""
        if userid/=""  then return (Just userid)  else do
        -- ���� �� ���������� - ������ ���� ���������� ����������� �� Windows Registry...
        userid <- do userid <- registryGetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID"
                     case userid of
                       Just userid -> return userid
                                      -- ... ��� � ������� ������ - ������� �����
                       Nothing     -> generateRandomBytes 8 >>== encode16
        -- � ���������� ��� �������
        registrySetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID" userid
        fmReplaceHistory fm' "UserID" userid
        -- ���������� ��� ������ ���� ������ � ���-���� ���� ��������
        userid1 <- fmGetHistory1 fm' "UserID" ""
        return (if userid==userid1  then Just userid  else Nothing)

  -- ���������� True ��� � �����
  let daily = do
        last <- fmGetHistory1 fm' "LastCheck" ""
        now  <- getUnixTime
        let day = round$ 24.37*60*60
        if  last>""  &&  (now - readI last < day)  then return False  else do
        fmReplaceHistory fm' "LastCheck" (show now)
        now1 <- fmGetHistory1 fm' "LastCheck" ""
        return (show now==now1)

  -- Size of maximum memory block we can allocate in bytes
  maxBlock <- getMaxMemToAlloc

  -- ������������ ������������� ��������� � ��������� �������
  --  (manual=True - ������ ����� �� ����, False - ���������� ������������)
  let checkNews manual = do
        postGUIAsync$ fmStackMsg fm' "0295 Checking for updates..."
        forkIO_ $ do
          -- ������� �� ������������� ���������
          whenJustM_ getUserID $ \userid -> do
#ifdef FREEARC_WIN
            si <- getSystemInfo; let ramLimit = showMem (si.$siMaximumApplicationAddress.$ptrToWordPtr.$toInteger `roundTo` (4*mb))
#endif
            language <- i18n"0000 English"
            let url = aARC_WEBSITE ++ "/CheckNews.aspx?user=" ++ userid ++ "&version=" ++ urlEncode aARC_VERSION
                                   ++ "&OS%20family=" ++ iif isWindows "Windows" "Unix"
                                   ++ "&RAM=" ++ showMem (toInteger getPhysicalMemory `roundTo` (4*mb))
#ifdef FREEARC_WIN
                                   ++ "&address%20space=" ++ ramLimit
#endif
                                   ++ "&largest%20memory%20block=" ++ showMem (maxBlock `roundDown` (100*mb))
                                   ++ "&number%20of%20cores=" ++ show getProcessorsCount
                                   ++ "&language=" ++ urlEncode language
            -- �������� ���������� � ��������� �������� ��������
            handleErrors
              -- ����������� ��� ������������� ��������
              (when manual$ postGUIAsync$ do
                  msg <- i18n"0296 Cannot open %1. Do you want to check the page with browser?"
                  whenM (askOkCancel window (format msg newsURL)) $ do
                    openWebsite newsURL)
              -- ������� ��������� ��������
              (fileGetBinary url >>== (`showHex` "").crc32) $ \new_crc -> do
            -- �������� �������� ������� ���������
            old_crc <- fmGetHistory1 fm' "news_crc" ""
            postGUIAsync$ do
            fmStackMsg fm' ""
            if (new_crc == old_crc) then do
               msg <- i18n"0297 Nothing new at %1"
               manual &&& fmInfoMsg fm' (format msg newsURL)
             else do
               fmReplaceHistory fm' "news_crc" new_crc
               msg <- i18n"0298 Found new information at %1! Open the page with browser?"
               whenM (askOkCancel window (format msg newsURL)) $ do
                 openWebsite newsURL

  -- ������ � ��� ��������� ���������� ��������
  forkIO_ $ do
    whenM (fmGetHistoryBool fm' "CheckNews" True) $ do
      foreverM $ do
        whenM daily $ do
          checkNews False
        sleepSeconds (30*60)


  -- ������ �� ������������� GUI
  GtkSigs.on helpAct actionActivated $ do
    openHelp "0256 FreeArc-GUI-Eng.htm"

  -- ������ �� ������������� ��������� ������
  GtkSigs.on helpCmdAct actionActivated $ do
    openHelp "0257 FreeArc036-eng.htm"

  -- �������� �������� ���������
  GtkSigs.on homepageAct actionActivated $ do
    openWebsite homeURL

  -- �������� �������� ���������
  GtkSigs.on openForumAct actionActivated $ do
    openWebsite forumURL

  -- �������� �������� ���������
  GtkSigs.on openWikiAct actionActivated $ do
    openWebsite wikiURL

  -- �������� ���������� �� �����
  GtkSigs.on whatsnewAct actionActivated $ do
    checkNews True

  -- ������ About
  GtkSigs.on aboutAct actionActivated $ do
    bracketCtrlBreak "aboutDialogDestroy" aboutDialogNew widgetDestroy $ \dialog -> do
    dialog `set` [windowTransientFor   := window
                 ,aboutDialogName      := aARC_NAME
                 ,aboutDialogVersion   := aARC_VERSION_WITH_DATE
                 ,aboutDialogCopyright := "(c) "++aARC_EMAIL
                 ,aboutDialogComments  := unlines aARC_LICENSE
                 ,aboutDialogWebsite   := homeURL
                 ,aboutDialogAuthors   := ["Igor Pavlov (author of 7-zip, LZMA and EXE filter)"
                                          ,"Dmitry Shkarin (author of PPMd)"
                                          ,"Ilya Grebnov (author of GRZipII and LZP filter)"
                                          ,"Alexander Djourik and Pavel Zhilin (authors of TTA)"
                                          ,"Dmitry Subbotin (author of Carryless rangecoder)"
                                          ,"Joachim Henke (coauthor of Tornado)"
                                          ,"Mark Shevchenko (author of GUI SFX and web site)"
                                          ,aARC_EMAIL++" (author of remaining parts)"
                 ]]
    dialogRun dialog
    return ()

  -- �������� ��������� URL � ������� About
  -- aboutDialogSetUrlHook openWebsite

  -- �������������� ��������� ����-��������� ���������/�������, �������� � ��������� ������ (��� ��� ���������� - ������� ���������)
  terminateOnError $
    chdir fm' (head (args++["."]))
  fmStatusBarTotals fm'
  widgetShowAll window

