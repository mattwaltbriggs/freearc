{-# OPTIONS_GHC -cpp -XNondecreasingIndentation #-}
----------------------------------------------------------------------------------------------------
---- �������� ������ ���������.                                                                 ----
---- �������� parseCmdline �� ������ Cmdline ��� ������� ��������� ������ � ��������� ������    ----
----   ���������� �������.                                                                      ----
---- ���� ������� ������ ���������� ��������� �������, �� find_archives ��������� �            ----
----   ��� ������� �� ���.                                                                      ----
---- ����� ������ ������� �������� � ���������� ����� �� ��������� �����:                       ----
---- * ��������� ������  � �������  runArchiveCreate   �� ������ ArcCreate   (������� a/f/m/u/j/d/ch/c/k/rr)
---- * ���������� ������         -  runArchiveExtract  -         ArcExtract  (������� t/e/x)    ----
---- * ��������� �������� ������ -  runArchiveList     -         ArcList     (������� l/v)      ----
---- * �������������� ������     -  runArchiveRecovery -         ArcRecover  (������� r)        ----
---- ������� ���������� ��������� � ������������ �� ���������� ���������� ����������� �������.  ----
----                                                                                            ----
---- ��� ��������� � ���� ������� ����� ��� �������� ���������� � �������:                      ----
----   ArhiveFileList   - ��� ������ �� �������� ������������ ������                            ----
----   ArhiveDirectory  - ��� ������/������ ���������� ������                                   ----
----   ArhiveStructure  - ��� ������ �� ���������� ������                                       ----
----   ByteStream       - ��� ����������� �������� ������ � ������������������ ������           ----
----   Compression      - ��� ������ ���������� ��������, ���������� � ���������� CRC           ----
----   UI               - ��� �������������� ������������ � ���� ����������� ����� :)           ----
----   Errors           - ��� ������������ � ��������� ������� � ������ � �������               ----
----   FileInfo         - ��� ������ ������ �� ����� � ��������� ���������� � ���               ----
----   Files            - ��� ���� �������� � ������� �� ����� � ������� ������                 ----
----   Process          - ��� ���������� ��������� �� ������������ ����������������� ��������   ----
----   Utils            - ��� ���� ��������� ��������������� �������                            ----
----------------------------------------------------------------------------------------------------
module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import System.Mem
import System.IO

import Utils
import Process
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UI
import ArcCreate
import ArcExtract
import ArcRecover
#ifdef FREEARC_GUI
import FileManager
#endif


-- |������� ������� ���������
main         =  (doMain =<< myGetArgs) >> shutdown "" aEXIT_CODE_SUCCESS
-- |����������� ������� ������� ��� ������������� �������
arc cmdline  =  doMain (words cmdline)

-- |���������� ��������� ������ � ����� ������ � ��������� ��
doMain args  = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
#ifdef FREEARC_GUI
  -- GUI mode: run directly on main thread (GTK+ requires it on macOS)
  args <- processCmdfile args
  luaLevel "Program" [("command", unwords args)] $ do
  parseGUIcommands run args $ \args -> do
    uiStartProgram
    commands <- parseCmdline args
    mapM_ run commands
    uiDoneProgram
#else
  bg $ do
  -- setUncaughtExceptionHandler removed in GHC 9; use default behavior
  return ()
  setCtrlBreakHandler $ do
  ensureCtrlBreak "resetConsoleTitle" (resetConsoleTitle) $ do
  args <- processCmdfile args
  luaLevel "Program" [("command", unwords args)] $ do
  uiStartProgram
  commands <- parseCmdline args
  mapM_ run commands
  uiDoneProgram
#endif

 where
  handler ex  = do
#ifdef FREEARC_GUI
    doNothing0
#else
    whenM (val programFinished) $ do
      foreverM$ sleepSeconds 1      -- ���� ��������� ��������� � shutdown, �������� ��� ��������� ���������
    registerError$ GENERAL_ERROR$
      case ex of
        _ | show ex == "Deadlock" -> ["0011 No threads to run: infinite loop or deadlock?"]
        ErrorCall s -> [s]
        other       -> [show ex]
#endif


-- |�������������� ������� � ���������� � ���������� ��� ������� ����������� ������
run command@Command
                { cmd_name            = cmd
                , cmd_setup_command   = setup_command
                , opt_scan_subdirs    = scan_subdirs
                } = do
  performGC       -- ��������� ����� ����� ��������� ���������� ������
  setup_command   -- ��������� ���������, ����������� ����� ������� ���������� �������
  luaLevel "Command" [("command", cmd)] $ do
  case (cmd) of
    "create" -> find_archives  False           run_add     command
    "a"      -> find_archives  False           run_add     command
    "f"      -> find_archives  False           run_add     command
    "m"      -> find_archives  False           run_add     command
    "mf"     -> find_archives  False           run_add     command
    "u"      -> find_archives  False           run_add     command
    "j"      -> find_archives  False           run_join    command
    "cw"     -> find_archives  False           run_cw      command
    "ch"     -> find_archives  scan_subdirs    run_copy    command
    's':_    -> find_archives  scan_subdirs    run_copy    command
    "c"      -> find_archives  scan_subdirs    run_copy    command
    "k"      -> find_archives  scan_subdirs    run_copy    command
    'r':'r':_-> find_archives  scan_subdirs    run_copy    command
    "r"      -> find_archives  scan_subdirs    run_recover command
    "d"      -> find_archives  scan_subdirs    run_delete  command
    "e"      -> find_archives  scan_subdirs    run_extract command
    "x"      -> find_archives  scan_subdirs    run_extract command
    "t"      -> find_archives  scan_subdirs    run_test    command
    "l"      -> find_archives  scan_subdirs    run_list    command
    "lb"     -> find_archives  scan_subdirs    run_list    command
    "lt"     -> find_archives  scan_subdirs    run_list    command
    "v"      -> find_archives  scan_subdirs    run_list    command
    _ -> registerError$ UNKNOWN_CMD cmd aLL_COMMANDS


-- |���� ������, ���������� ��� ����� arcspec, � ��������� �������� ������� �� ������ �� ���
find_archives scan_subdirs   -- ������ ������ � � ������������?
              run_command    -- ���������, ������� ����� ��������� �� ������ ��������� ������
              command@Command {cmd_arcspec = arcspec} = do
  uiStartCommand command   -- ������� ������ ���������� �������
  arclist <- if scan_subdirs || is_wildcard arcspec
               then find_files scan_subdirs arcspec >>== map diskName
               else return [arcspec]
  results <- foreach arclist $ \arcname -> do
    performGC   -- ��������� ����� ����� ��������� ���������� �������
    luaLevel "Archive" [("arcname", arcname)] $ do
    -- ���� ������� ����� -ad, �� �������� � �������� �������� �� ����� ��� ������ (��� ����������)
    let add_dir  =  opt_add_dir command  &&&  (</> takeBaseName arcname)
    run_command command { cmd_arcspec      = error "find_archives:cmd_arcspec undefined"  -- cmd_arcspec ��� ������ �� �����������.
                        , cmd_arclist      = arclist
                        , cmd_arcname      = arcname
                        , opt_disk_basedir = add_dir (opt_disk_basedir command)
                        }
  uiDoneCommand command results   -- �������� � ����������� ���������� ������� ��� ����� ��������


-- |������� ���������� � �����: create, a, f, m, u
run_add cmd = do
  msg <- i18n"0246 Found %1 files"
  let diskfiles =  find_and_filter_files (cmd_filespecs cmd) (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = opt_include_dirs     cmd
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_group_f        = opt_find_group       cmd.$Just
                                , ff_arc_basedir    = opt_arc_basedir      cmd
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_diskfiles      = diskfiles     -- �����, ������� ����� �������� � �����
                   , cmd_archive_filter = const True }  -- ������ ������ ������ �� ����������� �������


-- |������� ������� �������: j
run_join cmd@Command { cmd_filespecs = filespecs
                       , opt_noarcext  = noarcext
                       } = do
  msg <- i18n"0247 Found %1 archives"
  let arcspecs  =  map (addArcExtension noarcext) filespecs   -- ������� � ������ ���������� �� ��������� (".arc")
      arcnames  =  map diskName ==<< find_and_filter_files arcspecs (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = Just False
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_group_f        = Nothing
                                , ff_arc_basedir    = ""
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_added_arcnames = arcnames      -- �������������� ������� ������
                   , cmd_archive_filter = const True }  -- ������ ������ ������ �� ����������� �������


-- |������� ����������� ������ � ��������� ���������: ch, c, k. s, rr
run_copy    = runArchiveAdd                    . setArcFilter full_file_filter
-- |������� �������� �� ������: d
run_delete  = runArchiveAdd                    . setArcFilter ((not.).full_file_filter)
-- |������� ���������� �� ������: e, x
run_extract = runArchiveExtract pretestArchive . setArcFilter (test_dirs extract_file_filter)
-- |������� ������������ ������: t
run_test    = runArchiveExtract pretestArchive . setArcFilter (test_dirs full_file_filter)
-- |������� ��������� �������� ������: l, v
run_list    = runArchiveList pretestArchive    . setArcFilter (test_dirs full_file_filter)
-- |������� ������ ��������� ����������� � ����: cw
run_cw      = runCommentWrite
-- |������� �������������� ������: r
run_recover = runArchiveRecovery

-- |Just shortcut
runArchiveAdd  =  runArchiveCreate pretestArchive writeRecoveryBlocks

{-# NOINLINE find_archives #-}
{-# NOINLINE run_add #-}
{-# NOINLINE run_join #-}
{-# NOINLINE run_copy #-}
{-# NOINLINE run_delete #-}
{-# NOINLINE run_extract #-}
{-# NOINLINE run_test #-}
{-# NOINLINE run_list #-}


----------------------------------------------------------------------------------------------------
---- �������� ������ ������, ���������� ���������, ��� ��������� ����� ������ ----------------------
----------------------------------------------------------------------------------------------------

-- |���������� � cmd �������� ������ �� ������ �������������� ������
setArcFilter filter cmd  =  cmd {cmd_archive_filter = filter cmd}

-- |�������� ����� � ������������ � �������� opt_file_filter, �� �����������
-- �������������� ���� �������� ������� � ��������� ������, ����������� ��� ���������
add_file_filter cmd      =  all_functions [opt_file_filter cmd, not.overwrite_f cmd]

-- |�������� ����� � ������������ � �������� full_file_filter, �� �����������
-- �������������� ���� �������� ������� � ��������� ������, ����������� ��� ���������
extract_file_filter cmd  =  all_functions [full_file_filter cmd, not.overwrite_f cmd]

-- |�������� ����� ������, ����� ������� ������� � ��������� ������,
-- ��������������� ������� opt_file_filter
full_file_filter cmd  =  all_functions
                           [  match_filespecs (opt_match_with cmd) (cmd_filespecs cmd) . fiFilteredName
                           ,  opt_file_filter cmd
                           ]

-- |�������� �������������� ������ � ��������� �����, ����������� ��� ���������,
-- � ����� �����, ������� ����� �� ������������ ��� ����������
overwrite_f cmd  =  in_arclist_or_temparc . fiDiskName
  where in_arclist_or_temparc filename =
            fpFullname filename `elem` cmd_arclist cmd
            || all_functions [(temparc_prefix `isPrefixOf`), (temparc_suffix `isSuffixOf`)]
                             (fpBasename filename)

-- |�������� � ������ ������ ������ `filter_f` ����� ��������� � ������������ � ������� ������� `cmd`
test_dirs filter_f cmd fi  =  if fiIsDir fi
                                then opt_x_include_dirs cmd
                                else filter_f cmd fi

