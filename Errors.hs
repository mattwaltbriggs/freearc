{-# OPTIONS_GHC -cpp #-}
---------------------------------------------------------------------------------------------------
---- ����������� ������/�������������� � ������ ��������� � ���. ----------------------------------
---------------------------------------------------------------------------------------------------
module Errors where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import Data.IORef
import System.Exit
import System.IO
import System.IO.Unsafe
#if defined(FREEARC_WIN)
import GHC.ConsoleHandler
#else
import System.Posix.Signals
#endif

import CompressionLib   (compressionLib_cleanup)
import Utils
import Files
import Charsets

-- |���� �������� ���������
aEXIT_CODE_SUCCESS      = 0
aEXIT_CODE_WARNINGS     = 1
aEXIT_CODE_FATAL_ERROR  = 2
aEXIT_CODE_BAD_PASSWORD = 21
aEXIT_CODE_USER_BREAK   = 255

-- |��� ��������� ���� ������ � ��������������
data ErrorTypes = GENERAL_ERROR                 [String]
                | CMDLINE_GENERAL               [String]
                | CMDLINE_SYNTAX                String
                | CMDLINE_INCOMPATIBLE_OPTIONS  String String
                | CMDLINE_NO_COMMAND            [String]
                | CMDLINE_NO_ARCSPEC            [String]
                | CMDLINE_NO_FILENAMES          [String]
                | UNKNOWN_CMD                   String [String]
                | CMDLINE_UNKNOWN_OPTION        String
                | CMDLINE_AMBIGUOUS_OPTION      String [String]
                | CMDLINE_BAD_OPTION_FORMAT     String
                | INVALID_OPTION_VALUE          String String [String]
                | CANT_READ_DIRECTORY           String
                | CANT_GET_FILEINFO             String
                | CANT_OPEN_FILE                String
                | BAD_CRC                       String
                | BAD_CFG_SECTION               String [String]
                | OP_TERMINATED
                | TERMINATED
                | NOFILES
                | SKIPPED_FAKE_FILES            Int
                | BROKEN_ARCHIVE                FilePath [String]
                | INTERNAL_ERROR                String
                | COMPRESSION_ERROR             [String]
                | BAD_PASSWORD                  FilePath FilePath
  deriving (Eq)


--foreign import "&errCounter" :: Ptr Int
{-
data SqliteException = SqliteException Int String
  deriving (Typeable)

catchSqlite :: IO a -> (SqliteException -> IO a) -> IO a
catchSqlite = catchDyn

throwSqlite :: SqliteException -> a
throwSqlite = throwDyn
-}

---------------------------------------------------------------------------------------------------
---- ��������� Ctrl-Break, Close � �.�. ������� ������� -------------------------------------------
---------------------------------------------------------------------------------------------------

setCtrlBreakHandler action = do
  --myThread <- myThreadId
  -- ��� ������ ��� ������������� ���������� ����������� ���������� ���������� �������
#if defined(FREEARC_WIN)
  bracket (installHandler$ Catch onBreak) (installHandler) $  \oldHandler -> do
    action
#else
  let catchSignals a  =  installHandler sigINT (CatchOnce$ onBreak undefined) Nothing
  bracket (catchSignals (CatchOnce$ onBreak (error "onBreak"))) (catchSignals) $  \oldHandler -> do
    action
#endif

-- |������� fail, ���� ���������� ���� ���������� ���������� ���������
failOnTerminated = do
  whenM (val operationTerminated) $ do
    fail ""

-- |��������� Ctrl-Break � ������� �� Cancel �������� � ���������� ������������� �
-- ��������� ����. �����, ������� ����������� ����������, ����������� �� ��
onBreak event = terminateOperation
terminateOperation = do
  isFM <- val fileManagerMode
  registerError$ iif isFM OP_TERMINATED TERMINATED

-- |������������� ��������� ���������� ��������� � �������� exitCode � ������� ��������� msg
shutdown msg exitCode = do
  w <- val warnings
  -- Make cleanup unless this is a second call (after pause)
  unlessM (val programFinished) $ do
    programFinished =: True
    separator' =: ("","\n")
    log_separator' =: "\n"

    fin <- val finalizers
    for fin $ \(name,id,action) -> do
      ignoreErrors$ action
    compressionLib_cleanup

    unlessM (val fileManagerMode) $ do
      case w of
        0 -> when (exitCode==aEXIT_CODE_SUCCESS) $ condPrintLineLn "k" "All OK"
        _ -> condPrintLineLn "n"$ "There were "++show w++" warning(s)"
      ignoreErrors (msg &&& condPrintLineLn "n" msg)
      condPrintLineLn "e" ""
#if !defined(FREEARC_WIN) && !defined(FREEARC_GUI)
    putStrLn ""  -- � Unix ����������� �������������� ������� ������ � ��������� �� ���������� ���������
#endif

    ignoreErrors$ closeLogFile
    ignoreErrors$ hFlush stdout
    ignoreErrors$ hFlush stderr
    --killThread myThread

    -- Make a pause if necessary
    when (exitCode/=aEXIT_CODE_USER_BREAK) $ do
      warningsBefore' <- val warningsBefore
      pause_option <- val pause_before_exit
      pause <- val pauseAction
      pause `on` case pause_option of
                   "on"          -> True
                   "off"         -> False
                   "on-warnings" -> w>warningsBefore' || exitCode/=aEXIT_CODE_SUCCESS
                   "on-error"    -> exitCode/=aEXIT_CODE_SUCCESS
                   _             -> False

  -- And finally - exit program!
  exit (exitCode  |||  (w &&& aEXIT_CODE_WARNINGS))
#if 0
  -- ����� ���������� ������ ���������� ���������, � ��������� arc.exe � ��� ������ ������
  exitWith$ case () of
   _ | exitCode>0 -> ExitFailure exitCode
     | w>0        -> ExitFailure aEXIT_CODE_WARNINGS
     | otherwise  -> ExitSuccess
#endif
  return undefined

-- |"handle" � ����������� "onException" ����� ��� ^Break
handleCtrlBreak name onException action = do
  failOnTerminated
  id <- newId
  handle (\(e :: SomeException) -> do onException; throwIO e) $ do
    bracket_ (addFinalizer name id onException)
             (removeFinalizer id)
             (action)

-- |"bracket" � ����������� "close" ����� ��� ^Break
bracketCtrlBreak name init close action = do
  failOnTerminated
  id <- newId
  bracket (do x<-init; addFinalizer name id (close x); return x)
          (\x -> do removeFinalizer      id; close x)
          action

-- |bracketCtrlBreak, ����������� fail ��� �������� Nothing �� init
bracketCtrlBreakMaybe name init fail close action = do
  bracketCtrlBreak name (do x<-init; when (isNothing x) fail; return x)
                        (`whenJust_` close)
                        (`whenJust`  action)

-- |��������� close-�������� �� ���������� action
ensureCtrlBreak name close action  =  bracketCtrlBreak name (return ()) (\_->close) (\_->action)

-- ��������/������� finalizer � ������
addFinalizer name id action  =  finalizers .= ((name,id,action):)
removeFinalizer id           =  finalizers .= filter ((/=id).snd3)
newId                        =  do curId+=1; id<-val curId; return id

-- |���������� �����
curId :: IORef Int
curId = unsafePerformIO (ref 0)
{-# NOINLINE curId #-}

-- |������ ��������, ������� ���� ��������� ����� ��������� ����������� ���������
finalizers :: IORef [(String, Int, IO ())]
finalizers = unsafePerformIO (ref [])
{-# NOINLINE finalizers #-}

-- |�� ��������� ����� �������� (������ � ������ ����-���������)
parent_id :: IORef ThreadId
parent_id = unsafePerformIO (ref undefined)
{-# NOINLINE parent_id #-}

-- |����, ������������ ��� �� ��������� � ������ ���������� ������� ��������
operationTerminated = unsafePerformIO (ref False)
{-# NOINLINE operationTerminated #-}

-- |��������������� ����� ���������� ���������� ���� ������, ����� �� ������ ��� �������� ���� ���������
programFinished = unsafePerformIO (ref False)
{-# NOINLINE programFinished #-}

-- |����� ������ ����-���������: ��� ���� registerError �������������� ��-������� - �� ���������� ���������� ���� ������ �������� � ����������
fileManagerMode = unsafePerformIO (ref False)
{-# NOINLINE fileManagerMode #-}

-- |������ �� ����� ����� ������� �� ���������?
pause_before_exit = unsafePerformIO (ref "")
{-# NOINLINE pause_before_exit #-}

-- |UI-��������, ���������� ��� �������� ������ �� ���������
pauseAction = unsafePerformIO (ref$ return ()) :: IORef (IO())
{-# NOINLINE pauseAction #-}


---------------------------------------------------------------------------------------------------
---- ������ ��������� � ��������� ����� ������. ���������� ������ ��� �������������� --------------
---------------------------------------------------------------------------------------------------

errormsg (GENERAL_ERROR msgs) =
  i18fmt msgs

errormsg (BROKEN_ARCHIVE arcname msgs) = do
  msg <- i18fmt msgs
  i18fmt ["0341 %1 isn't archive or this archive is corrupt: %2. Please recover it using 'r' command or use -tp- option to ignore Recovery Record", arcname, msg]

errormsg (INTERNAL_ERROR msg) =
  return$ "FreeArc internal error: "++msg

errormsg (COMPRESSION_ERROR msgs) =
  i18fmt msgs

errormsg (CMDLINE_GENERAL msgs) =
  i18fmt msgs

errormsg (CMDLINE_SYNTAX syntax) =
  i18fmt ["0318 command syntax is \"%1\"", syntax]

errormsg (CMDLINE_INCOMPATIBLE_OPTIONS option1 option2) =
  i18fmt ["0319 options %1 and %2 can't be used together", option1, option2]

errormsg (UNKNOWN_CMD cmd known_cmds) =
  i18fmt ["0320 unknown command \"%1\". Supported commands are: %2", cmd, joinWith ", " known_cmds]

errormsg (CMDLINE_UNKNOWN_OPTION option) =
  i18fmt ["0321 unknown option \"%1\"", option]

errormsg (CMDLINE_AMBIGUOUS_OPTION option variants) = do
  or <- i18n"0323 or"
  i18fmt ["0322 ambiguous option \"%1\" - is that %2?", option, enumerate or variants]

errormsg (CMDLINE_BAD_OPTION_FORMAT option) =
  i18fmt ["0325 option \"%1\" have illegal format", option]

errormsg (INVALID_OPTION_VALUE fullname shortname valid_values) = do
  or <- i18n"0323 or"
  let spelling | shortname>"" = (('-':shortname)++)
               | otherwise    = (("--"++fullname++"=")++)
  i18fmt ["0326 %1 option must be one of: %2", fullname, enumerate or (map spelling valid_values)]

errormsg (CMDLINE_NO_COMMAND args) =
  i18fmt ["0327 no command name in command: %1", unwords args]

errormsg (CMDLINE_NO_ARCSPEC args) =
  i18fmt ["0328 no archive name in command: %1", unwords args]

errormsg (CMDLINE_NO_FILENAMES args) =
  i18fmt ["0329 no filenames in command: %1", unwords args]

errormsg (CANT_READ_DIRECTORY dir) =
  i18fmt ["0330 can't read directory \"%1\"", dir]

errormsg (CANT_GET_FILEINFO filename) =
  i18fmt ["0331 can't get info about file \"%1\"", filename]

errormsg (CANT_OPEN_FILE filename) =
  i18fmt ["0332 can't open file \"%1\"", filename]

errormsg (BAD_CRC filename) =
  i18fmt ["0333 CRC error in file \"%1\"", filename]

errormsg (BAD_CFG_SECTION cfgfile section) =
  i18fmt ["0334 bad section %1 in %2", head section, cfgfile]

errormsg (OP_TERMINATED) =
  i18fmt ["0455 Operation terminated by user!"]

errormsg (TERMINATED) =
  i18fmt ["0456 Program terminated by user!"]

errormsg (NOFILES) =
  i18fmt ["0337 no files, erasing empty archive"]

errormsg (SKIPPED_FAKE_FILES n) =
  i18fmt ["0338 skipped %1 fake files", show n]

errormsg (BAD_PASSWORD archive "") =
  i18fmt ["0339 bad password for archive %1", archive]

errormsg (BAD_PASSWORD archive file) =
  i18fmt ["0340 bad password for %1 in archive %2", file, archive]


-- |����������� ������ ��������
enumerate s list  =  joinWith2 ", " (" "++s++" ") (map quote list)

{-# NOINLINE errormsg #-}


----------------------------------------------------------------------------------------------------
---- ���� ������ ��� ��������� ������ --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

errcode TERMINATED     = aEXIT_CODE_USER_BREAK
errcode BAD_PASSWORD{} = aEXIT_CODE_BAD_PASSWORD
errcode _              = aEXIT_CODE_FATAL_ERROR


----------------------------------------------------------------------------------------------------
---- ����/����� �� ����� � ���������, �������� ������ -sct -----------------------------------------
----------------------------------------------------------------------------------------------------

#ifdef FREEARC_GUI
myPutStr      = doNothing
myPutStrLn    = doNothing
myFlushStdout = doNothing0
#else
myGetLine     = getLine >>= terminal2str
myPutStr      = putStr   =<<. str2terminal
myPutStrLn    = putStrLn =<<. str2terminal
myFlushStdout = hFlush stdout
#endif


----------------------------------------------------------------------------------------------------
---- ������ � ��������� � ���������� ������� ������ �� ����� � ������������ � ������ --display -----
----------------------------------------------------------------------------------------------------

-- ���������� �������� ������, ������� � ��� ������������� �� ���������� �������/������������� ������
-- ����� ����, ������ ����� ���������� ������ ����������� � ������ �������,
-- ���� ��� ���������� ��������������� ����� ��������� ���������
printLine = printLineC ""
printLineC c str = do
  (oldc,separator) <- val separator'
  let makeLower (x:y:zs) | isLower y  =  toLower x:y:zs
      makeLower xs                    =  xs
  let handle "w" = stderr
      handle _   = stdout
#ifndef FREEARC_GUI
  hPutStr (handle oldc) =<< str2terminal separator
  hPutStr (handle c)    =<< str2terminal ((oldc=="h" &&& makeLower) str)
  hFlush  (handle c)
#endif
  separator' =: (c,"")

-- |���������� ������ � ������������ ����� ����� ��
printLineLn str = do
  printLine str
  printLineNeedSeparator "\n"

-- �������� ����������� ����� �������� �������. �� ������� ��� ������ �����,
-- ��������� �������� ������������ ������ ����� � �� ���� :)))
printLineNeedSeparator str = do
  separator' =: ("",str)

-- �������� ������ � �������.
-- ������� � �� ����� ��� �������, ��� � ����� �� �������� ������ --display
condPrintLine c line = do
  if c=="G" then val loggingHandlers >>= mapM_ ($line) else do
    display_option :: String <- val display_option'
    when (c/="$" || (display_option `contains` '#')) $ do
        printLog line
    when (display_option `contains_one_of` c) $ do
        printLineC c line

-- |���������� ������ � ������������ ����� ����� ��
condPrintLineLn c line = do
  condPrintLine c line
  condPrintLineNeedSeparator c "\n"

-- �������� ����������� ����� �������� ������� ��� ������� ���������� ������ ������ c
condPrintLineNeedSeparator c str = do
  display_option :: String <- val display_option'
  when (c/="$" || (display_option `contains` '#')) $ do
      log_separator' =: str
  when (c=="" || (display_option `contains_one_of` c)) $ do
      separator' =: (c,str)

-- ������� �������
openLogFile logfilename = do
  closeLogFile  -- ������� ����������, ���� ���
  logfile <- case logfilename of
                 ""  -> return Nothing
                 log -> fileAppendText log >>== Just
  logfile' =: logfile

-- ������� ������ � �������
printLog line = do
  separator <- val log_separator'
  whenJustM_ (val logfile') $ \log -> do
      fileWrite log =<< str2logfile (separator ++ line); fileFlush log
      log_separator' =: ""

-- ������� �������
closeLogFile = do
  whenJustM_ (val logfile') fileClose
  logfile' =: Nothing

-- ����������, �������� Handle ��������
logfile'        = unsafePerformIO$ newIORef Nothing
-- ����������, ������������ ��� ��������� ������
separator'      = unsafePerformIO$ newIORef ("","") :: IORef (String,String)
log_separator'  = unsafePerformIO$ newIORef "\n"    :: IORef String
display_option' = unsafePerformIO$ newIORef$ error "undefined display_option"
-- �������� ������ ��������� � ���
loggingHandlers = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]

{-# NOINLINE printLine #-}
{-# NOINLINE printLineNeedSeparator #-}
{-# NOINLINE condPrintLine #-}
{-# NOINLINE condPrintLineNeedSeparator #-}
{-# NOINLINE separator' #-}
{-# NOINLINE log_separator' #-}
{-# NOINLINE display_option' #-}

----------------------------------------------------------------------------------------------------
---- ������ ��������� �� ������� � ��������������
----------------------------------------------------------------------------------------------------

-- |������ ��������� �� ������ � ������� � ��������� ���������� ��������� � ���� ����������
registerError err = do
  msg <- errormsg err
  msg <- if err `elem` [TERMINATED,OP_TERMINATED]
           then return msg
           else i18fmt ["0316 ERROR: %1", msg]
  val errorHandlers >>= mapM_ ($msg)
  -- ���� �� �� � ������ ����-��������� - ��������� ��������� ����� �� ���������
  unlessM (val fileManagerMode) $ do
    shutdown msg (errcode err)
  -- ����� ��� ���������� ���� ������ ����������
  operationTerminated =: True
  killThread =<< val parent_id
  fail ""

-- |������ �������������� � ������� � ����� ��� �� �����
registerWarning warn = do
  warnings += 1
  msg <- errormsg warn
  msg <- i18fmt ["0317 WARNING: %1", msg]
  val warningHandlers >>= mapM_ ($msg)
  condPrintLineLn "w" msg

-- |��������� �������� � ���������� ���������� ��������� ��� ���� warning'��
count_warnings action = do
  w0 <- val warnings
  action
  w  <- val warnings
  return (w-w0)

-- |������� ������, ��������� � ���� ������ ���������
warnings = unsafePerformIO$ newIORef 0 :: IORef Int
-- |���������� �������������� ����� ��������� ������� ������� ���������
warningsBefore = unsafePerformIO$ newIORef 0 :: IORef Int

-- � ����������� �� ������ ���������������� ������ ��� ��������������
registerThreadError err = do
  isFM <- val fileManagerMode
  (iif isFM registerWarning registerError) err

-- ��������, ����������� ��� ��������� ������/�������������� (�������������� � ������ ������ ���������)
errorHandlers   = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]
warningHandlers = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]

{-# NOINLINE registerError #-}
{-# NOINLINE registerWarning #-}
{-# NOINLINE warnings #-}
{-# NOINLINE warningsBefore #-}
{-# NOINLINE errorHandlers #-}
{-# NOINLINE warningHandlers #-}

----------------------------------------------------------------------------------------------------
---- ������ � �������
----------------------------------------------------------------------------------------------------

-- |���������� Nothing � ���������� ��������� �� ������, ���� ���� �� ������� �������
tryOpen filename = (fileOpen filename >>== Just) `catch` \(_ :: IOException) -> do registerWarning$ CANT_OPEN_FILE filename; return Nothing

-- |����������� ����
fileCopy srcname dstname = do
  bracketCtrlBreak "fileClose1:fileCopy" (fileOpen srcname) (fileClose) $ \srcfile -> do
    handleCtrlBreak "fileRemove1:fileCopy" (ignoreErrors$ fileRemove dstname) $ do
      bracketCtrlBreak "fileClose2:fileCopy" (fileCreate dstname) (fileClose) $ \dstfile -> do
        size <- fileGetSize srcfile
        fileCopyBytes srcfile size dstfile


----------------------------------------------------------------------------------------------------
----- External functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Stop program execution
foreign import ccall unsafe "stdlib.h exit"
  exit :: Int -> IO ()

