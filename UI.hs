{-# OPTIONS_GHC -cpp -XNondecreasingIndentation #-}
----------------------------------------------------------------------------------------------------
---- ���� � ����������� ���������� ������ ��������� (����� ������������ ������, �������� � �.�.) ---
----------------------------------------------------------------------------------------------------
#ifdef FREEARC_GUI
module UI (module UI, module UIBase, module GUI) where
import GUI
#else
module UI (module UI, module UIBase, module CUI) where
import CUI
#endif

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Ratio
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.IO.Unsafe
import System.Time

import Utils
import Errors
import Charsets
import Files
import FileInfo
import Compression (encode_method, showMem, getCompressionMem, getDecompressionMem)
import Options
import UIBase


-- |�������� ������ ���������� ���������
uiStartProgram = do
  guiStartProgram

-- |�������� ������ ���������� �������
uiStartCommand command = do
  pauseAction =: guiPauseAtEnd
  ref_command =: command
  pause_before_exit =: opt_pause_before_exit command
  display_option' =: opt_display command
  refStartArchiveTime =:: getClockTime
  -- ������� ������� � ������� � ���� ����������� �������. ������� �����������/������ ������/����� ������
  -- �� ������ �������� � ���-����, ��� ��� �� �������� ������ � ��� ������ � ��� �� 100 ���������
  openLogFile (opt_logfile command)
  curdir <- getCurrentDirectory
  exe <- getExeName
  printLog (curdir++">"++takeBaseName exe++" "++unwords(map (takeSome 100 "...")$ hidePasswords$ takeSome 100 ["..."]$ cmd_args command)++"\n")
  -- ������� ������ ���������� � ������������ �������������� �����
  let addArgs = cmd_additional_args command
  once putHeader$ condPrintLine "h" aARC_HEADER
  condPrintLine "o" (addArgs &&& "Using additional options: "++unwords(hidePasswords addArgs)++"\n")
  myFlushStdout

-- |�������� ������ ���������� ����������
uiStartSubCommand command subCommand = do
  ref_command =: subCommand
  uiArcname   =: cmd_arcname command
  display_option' =: opt_display subCommand

-- |�������� ������ ��������� ���������� ������
uiStartArchive command@Command {
                 opt_data_compressor = compressor
               , opt_cache           = cache
               }
               method = do
  -- ��������� ����� ������ ��������� ������ � ����������� �������
  refStartArchiveTime =:: getClockTime
  ref_command =: command
  display_option' =: opt_display command
  uiMessage =: ""
  guiStartArchive

  -- ������� ��������� �� ����� ���������, ���� ��� ���-������� (��������, ������������ ����� ���������)
  if cmd_subcommand command
    then do condPrintLineNeedSeparator "" "\n"
    else do

  -- ������� ��������� ���� "Testing archive ..."
  let cmd     = cmd_name    command
      arcname = cmd_arcname command
  uiArcname =: arcname
  exist <- fileExist arcname
  msg <- i18n (msgStartGUI cmd exist)
  condPrintLine "G"  $ (format msg arcname)
  condPrintLine "a"  $ (msgStart cmd exist) ++ arcname
  condPrintLine "c"  $ (method &&& " using "++encode_method method)
  condPrintLine "ac" $ "\n"
  when (cmdType cmd == ADD_CMD) $ do
      condPrintLineLn "m" $
          "Memory for compression "++showMem (getCompressionMem   method)
          ++", decompression "     ++showMem (getDecompressionMem method)
          ++", cache "             ++showMem cache
  -- ��������� ���������� ��� ������������ �������������
  ref_w0 =:: val warnings
  ref_arcExist =: exist

-- |�������� ������ �������� ��� ���������� ������
uiStartProcessing filelist archive_total_bytes archive_total_compressed = do
  refArchiveProcessingTime =: 0
  command <- val ref_command
  let cmd = cmd_name command
      total_files' = i$ length filelist
      total_bytes' = sum (map fiSize filelist)
      ui_state = UI_State {
          total_files     = total_files'
        , total_bytes     = total_bytes'
        , archive_total_bytes      = archive_total_bytes
        , archive_total_compressed = archive_total_compressed
        , datatype        = error "internal CUI error: datatype not initialized"
        , uiFileinfo      = Nothing
        , files           = 0
        , bytes           = 0
        , cbytes          = 0
        , dirs            = 0
        , dir_bytes       = 0
        , dir_cbytes      = 0
        , fake_files      = 0
        , fake_bytes      = 0
        , fake_cbytes     = 0
        , algorithmsCount = error "internal CUI error: algorithmsCount not initialized"
        , rw_ops          = error "internal CUI error: rw_ops not initialized"
        , r_bytes         = error "internal CUI error: r_bytes not initialized"
        , rnum_bytes      = error "internal CUI error: rnum_bytes not initialized"
        }
  ref_ui_state =: ui_state
  printLine$ msgDo cmd ++ show_files3 total_files' ++ ", "
                       ++ show_bytes3 total_bytes'
  -- ����� ����� "�����������" �������� �������� �� ������ ������� � ������� �����������
  printLineNeedSeparator $ "\r"++replicate 75 ' '++"\r"
  when (opt_indicator command == "1") $ do
    myPutStr$ ". Processed "
  -- ������� progress indicator ��������� ����� ������� ������������ ������, ��� ��������� ��� �������� �������
  -- ������ � ������� ����������� �� �������: �������� �������� 1��/�, ����� �������� ����� - 10 ����
  let current bytes = do ui_state <- val ref_ui_state
                         return$ bytes + (bytes_per_sec `div` 100)*i (files ui_state)
      total = do ui_state <- val ref_ui_state
                 return$ total_bytes ui_state + (bytes_per_sec `div` 100)*i (total_files ui_state)
  uiStartProgressIndicator INDICATOR_FULL command current total
  myFlushStdout


-- |�������� ������ ���������� ��������
uiStage msg = do
  syncUI $ do
  uiMessage =:: i18n msg

-- |�������� ������� ���������������� ������
uiStartScanning = do
  files_scanned =: 0

-- |���������� � ���� ������������ �����, files - ������ ������, ��������� � ��������� ��������
uiScanning msg files = do  -- ���� ��� �������� ������ � GUI
#ifdef FREEARC_GUI
  failOnTerminated
  files_scanned += i(length files)
  files_scanned' <- val files_scanned
  msg <- i18n msg
  uiStage$ format msg (show3 files_scanned')
#endif
  return ()

-- |�������� ������ ��������/���������� ������
uiStartFiles count = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = File
             , algorithmsCount = count
             , rw_ops          = replicate count []
             , r_bytes         = 0
             , rnum_bytes      = 0
             }

-- |�������� ������ ��������/���������� �������� ������
uiStartDirectory = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = Dir
             , dirs            = dirs ui_state + 1
             , algorithmsCount = 0 }

-- |�������� ������ ��������/���������� ��������� ���������� ������
uiStartControlData = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = CData
             , algorithmsCount = 0 }

-- |�������� ������ ��������/���������� �����
uiStartFile fileinfo = do
  syncUI $ do
    uiMessage =: (fpFullname.fiStoredName) fileinfo  ++  (fiIsDir fileinfo &&& "/")
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { datatype   = File
               , uiFileinfo = Just fileinfo
               , files      = files ui_state + 1}
  guiStartFile

-- |���������������� total_bytes � ui_state
uiCorrectTotal files bytes = do
  when (files/=0 || bytes/=0) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { total_files = total_files ui_state + files
               , total_bytes = total_bytes ui_state + bytes }

-- |�������� �������� ��������� ������ �������� ������������ ������
uiFakeFiles filelist compsize = do
  let origsize  =  sum (map fiSize filelist)
  syncUI $ do
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { datatype    = File
               , files       = (files       ui_state) + (i$ length filelist)
               , fake_files  = (fake_files  ui_state) + (i$ length filelist)
               , fake_bytes  = (fake_bytes  ui_state) + origsize
               , fake_cbytes = (fake_cbytes ui_state) + compsize
               }
  uiUnpackedBytes           origsize
  uiCompressedBytes         compsize
  uiUpdateProgressIndicator origsize

-- |��������, ��� ���� ���������� �������-�� ���� ������ ������ (�������, ���������
-- �� ��� ��������, ������� ������ ��� ���������� ��� ������ ����������� ������, ����������
-- �� ������� ������ � ����� ��� �����-���� �����������)
uiCompressedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     cbytes =     cbytes ui_state + len }
      Dir   ->  ui_state { dir_cbytes = dir_cbytes ui_state + len }
      CData ->  ui_state

-- |��������, ��� ���� ���������� �������-�� ���� ������������� ������ (���� ���� �������
-- ��� ����� ����� � ����� �� �����, ��������� ��� ���� �������� � ������ ���� �� ������ � �����)
uiUnpackedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     bytes =     bytes ui_state + len }
      Dir   ->  ui_state { dir_bytes = dir_bytes ui_state + len }
      CData ->  ui_state

-- |�������� ������ �������� ��� ���������� �����-�����
uiStartDeCompression deCompression = do
  x <- getCPUTime
  newMVar (x,deCompression,[])

-- |�������� � ������ ����� ������ ������ �� ���������� � �������
-- (����������� � ������ ����� ����� ������ ����������/������������)
uiDeCompressionTime times t =  do
  modifyMVar_ times (\(x,y,ts) -> return (x, y, ts++[t]))

-- |��������/���������� �����-����� ��������� - �������������� ����� ������ ���� ������
-- ��� ������������ wall clock time, ���� ���� �� ���� �� ������������ ����� == -1
uiFinishDeCompression times = do
  (timeStarted, deCompression, results) <- takeMVar times
  timeFinished <- getCPUTime
  let deCompressionTimes  =  map snd3 results
  refArchiveProcessingTime +=  {-if (all (>=0) deCompressionTimes)      -- Commented out until all compression methods (lzma, grzip) will include timing for all threads
                                 then sum deCompressionTimes
                                 else-} i(timeFinished - timeStarted) / 1e12
  let total_times = if (all (>=0) deCompressionTimes)
                                 then " ("++showFFloat (Just 3) (sum deCompressionTimes) ""++" seconds)"
                                 else ""
  when (results>[]) $ do
    debugLog0$ "  Solid block "++deCompression++" results"++total_times
    for results $ \(method,time,size) -> do
        debugLog0$ "    "++method++": "++show3 size++" bytes in "++showFFloat (Just 3) time ""++" seconds"

-- |��������� ���������� ������ ��������� -> ���������� ���������� � ������� � ���������� ���������
uiDoneArchive = do
  command <- val ref_command
  ui_state@UI_State { total_files   = total_files
                      , total_bytes   = total_bytes
                      , files         = files
                      , bytes         = bytes
                      , cbytes        = cbytes
                      , dirs          = dirs
                      , dir_bytes     = dir_bytes
                      , dir_cbytes    = dir_cbytes
                      , fake_files    = fake_files
                      , fake_bytes    = fake_bytes
                      , fake_cbytes   = fake_cbytes }  <-  val ref_ui_state
  let cmd = cmd_name command
  uiMessage =: ""
  updateAllIndicators
  uiDoneProgressIndicator
  when (opt_indicator command=="2" && files-fake_files>0) $ do
    myPutStrLn ""
    printLineNeedSeparator ""  -- ����� � ����������� ����� ������� ��������� ����� �������

  -- ���������� ������ (�� ��������� ��� ���-������, ��������� ����� ����� �� ���������� ��� ���������� �������� �������)
  unless (cmd_subcommand command) $ do
    condPrintLineLn "f" $ left_justify 75 $    -- ��� �������������� �������� ����� �� ������������ ��������� ���������� ������
      msgDone cmd ++ show_files3 files ++ ", " ++ show_ratio cmd bytes cbytes
    -- ���������� ���������� �� �������� ������ ������ ���� �� ���������� �����
    when (dir_bytes>10^4) $ do
      condPrintLine   "d" $ "Directory " ++ (dirs>1 &&& "has " ++ show3 dirs ++ " chunks, ")
      condPrintLineLn "d" $                 show_ratio cmd dir_bytes dir_cbytes

  -- ���������� � ������� ������ � �������� ��������/����������
  secs <- val refArchiveProcessingTime   -- �����, ����������� ��������������� �� ��������/����������
  real_secs <- return_real_secs          -- ������ ����� ���������� ������� ��� ������� �������
  condPrintLine                          "t" $ msgStat cmd ++ "time: "++(secs>0 &&& "cpu " ++ showTime secs ++ ", ")
  condPrintLine                          "t" $ "real " ++ showTime real_secs
  when (real_secs>=0.01) $ condPrintLine "t" $ ". Speed " ++ showSpeed (bytes-fake_bytes) real_secs

  condPrintLineNeedSeparator "rdt" "\n"
  myFlushStdout
  resetConsoleTitle
  return (1,files,bytes,cbytes)

-- |���������� ����� ���� ��������������� �������� (���������� recovery info, ������������)
uiDoneArchive2 = do
  command <- val ref_command
  let cmd     = cmd_name    command
      arcname = cmd_arcname command
  w0 <- val ref_w0
  w1 <- val warnings
  let w = w1-w0  -- number of warnings while processing this archive
  unlessM (val operationTerminated) $ do
    arcExist <- val ref_arcExist
    msg <- i18n (msgFinishGUI cmd arcExist w)
    condPrintLine "G" (formatn msg [arcname, show w])
  unless (cmd_subcommand command) $ do
    condPrintLineNeedSeparator "" "\n\n"

-- |���������� ���������� ���������
uiDoneSubCommand command subCommand results = do
  ref_command =: command
  display_option' =: opt_display command

-- |���������� ������� ���������, ���������� ��������� ���������� �� ���� ������������ �������
uiDoneCommand Command{cmd_name=cmd} totals = do
  let sum4 (a0,b0,c0,d0) (a,b,c,d)   =  (a0+a,b0+b,c0+c,d0+d)
      (counts, files, bytes, cbytes) =  foldl sum4 (0,0,0,0) totals
  when (counts>1) $ do
    condPrintLine "s" $ "Total: "++show_archives3 counts++", "
                                 ++show_files3    files ++", "
                                 ++if (cbytes>=0)
                                     then show_ratio cmd bytes cbytes
                                     else show_bytes3 bytes
    condPrintLineNeedSeparator "s" "\n\n\n"

-- |��������� ���������� ���������
uiDoneProgram = do
  condPrintLineNeedSeparator "" "\n"
  guiDoneProgram


{-# NOINLINE uiStartProgram #-}
{-# NOINLINE uiStartArchive #-}
{-# NOINLINE uiStartProcessing #-}
{-# NOINLINE uiStartFile #-}
{-# NOINLINE uiCorrectTotal #-}
{-# NOINLINE uiUnpackedBytes #-}
{-# NOINLINE uiCompressedBytes #-}
{-# NOINLINE uiDoneArchive #-}
{-# NOINLINE uiDoneCommand #-}


----------------------------------------------------------------------------------------------------
---- ������� �������� r/w, �� ������� ��� �������� ����������� ��������� ��������� -----------------
----------------------------------------------------------------------------------------------------

-- �������� �������� ������/������ � ������ ������, ������ ������ �������� ������ ����
add_Read  a (UI_Write 0:UI_Read  0:ops) = (UI_Read a:ops)  -- ���������� �� useless ���� r0+w0
add_Read  a (UI_Read  b:ops) = (UI_Read (a+b):ops)
add_Read  a             ops  = (UI_Read  a   :ops)

add_Write a (UI_Write b:ops) = (UI_Write(a+b):ops)
add_Write a             ops  = (UI_Write a   :ops)

-- |�������� ����� num � ������� ������� �������� bytes ����, ��������������� ���������� ����� �����������
-- ������ (��� �������� "�������� ������" ��������� ������������ ���������� ��������� ���������)
uiQuasiWriteData num bytes = do
  -- ���������� �������� ���, ��� ��������� ����������� ������ �������������� bytes ���������� ������,
  -- �� ��� ���� ����� ������ ���������� ������ �� ���������� �� �� ���� ;)
  uiWriteData num bytes
  uiReadData  num 0
  uiWriteData num (-bytes)

-- |�������� ����� num � ������� ������� bytes ����
uiWriteData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- ��������� � ������ �������� �/� �������� ������
  when (num>=1 && num<count) $ do
    syncUI $ do
    ui_state@UI_State {rw_ops=rw_ops0}  <-  val ref_ui_state
    let rw_ops = updateAt num (add_Write bytes) rw_ops0
    return $! length (take 4 (rw_ops!!num))   -- strictify operations list!
    ref_ui_state =: ui_state {rw_ops=rw_ops}

-- |�������� ����� num � ������� �������� bytes ����
uiReadData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- ��������� � ������ �������� �/� �������� ������
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state@UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Read bytes) rw_ops}

  -- �������� ��������� ���������, ���� ��� ��������� �������� ������ � �������
  when (num>=1 && num==count) $ do
    unpBytes <- syncUI $ do
      -- ��������� �� ��������� ���� ����
      ui_state@UI_State {r_bytes=r_bytes0, rnum_bytes=rnum_bytes0, rw_ops=rw_ops0}  <-  val ref_ui_state
      -- � ������ �� ����� ��������� num ����������� bytes ����,
      -- ����������� ���������� ���� �� ����� ������� ��������� ���� ���� ���� �� ����� �� ������ ��������� (bytes>16)
      let rnum_bytes = rnum_bytes0+bytes
          (r_bytes, rw_ops) = if bytes>16
                                 then calc num (reverse rw_ops0) [] rnum_bytes
                                 else (r_bytes0, rw_ops0)
      ref_ui_state =: ui_state {r_bytes=r_bytes, rnum_bytes=rnum_bytes, rw_ops=rw_ops}
      --for rw_ops $ \x -> print (reverse x)
      --print (rnum_bytes0, bytes, r_bytes0, r_bytes-r_bytes0)
      -- ���������� ���������� ���� �� ����� ������� ��������� ������������ ����������� �������� ���� ��������
      return (r_bytes-r_bytes0)
    uiUpdateProgressIndicator (toRational unpBytes*9/10)
  when (num==1) $ do  -- 90% �� ��������� �������� � ������� � 10% �� ������ (����� �������� ����� ��� external compression and so on)
    uiUpdateProgressIndicator (toRational bytes/10)

 where
  -- ���������� ����������� bytes ���� �� ����� ��������� num � ���������� ���� �� ����� ��������� 1
  -- ������ �� �������� ������� ��������, ������������� �������� �������������� ������� ����� ��������
  calc 1   _                new_ops bytes = (bytes, []:new_ops)
  calc num (old_op:old_ops) new_ops bytes =
    -- ����������� bytes ���� �� ������ ��������� num-1 � ����� �� ��� �����
    let (new_bytes, new_op) = go 0 bytes (0,0) (smart_reverse old_op)
    in calc (num-1) old_ops (reverse new_op:new_ops) new_bytes

  -- ����������� oplist ��� ������ ������� ��� �� ��� ��������, ���� � ��� ������ 1000 ���������
  -- (�� ���� ������ ����� ��� �������� ����� tempfile)
  smart_reverse oplist
      | length oplist < 1000  =  reverse oplist
      | otherwise             =  [UI_Read r, UI_Write w]  where (r,w) = go oplist
                                                                go (UI_Read  r:ops) = mapFst (+r) (go ops)
                                                                go (UI_Write w:ops) = mapSnd (+w) (go ops)
                                                                go []               = (0,0)

  -- ������������� ���������� ����� (restW) � ����������� (totalR) �������� ������������������ �������� �����/������
  go totalR restW (rsum,wsum) ops@(UI_Read r:UI_Write w:rest_ops)
       -- ���� ��������� ����� ����������� ������ ������ ������ �������, �� ����������� totalR �� ���� � �������� ������
       | w<restW    =  go (totalR+r) (restW-w) (rsum+r,wsum+w) rest_ops
       -- ����� ����� ��� ��������������� (r/w * restW) � ��������� � totalR
       | otherwise  =  (totalR + ((r*restW) `div` max w 1), UI_Read rsum:UI_Write wsum:ops)
  -- ��� ������ ��������
  go totalR _ (rsum,wsum) ops  =  (totalR, UI_Read rsum:UI_Write wsum:ops)


----------------------------------------------------------------------------------------------------
---- ���������� ���������� ��������� ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���������������� ��������� ���������
uiStartProgressIndicator indType command bytes' total' = do
  bytes <- bytes' 0;  total <- total'
  arcname <- val uiArcname
  let cmd        =  cmd_name command
      direction  =  if (cmdType cmd == ADD_CMD)  then " => "  else " <= "
      indicator  =  select_indicator command total
  aProgressIndicatorState =: (indicator, indType, arcname, direction, 0 :: Rational, bytes', total')
  indicator_start_real_secs =:: return_real_secs
  uiResumeProgressIndicator

-- |������� �� ����� � � ��������� ���� ��������� ��������� (������� ��������� ������ ��� ����������)
uiUpdateProgressIndicator add_b =
  when (add_b/=0) $ do
    -- ��������� ��������� ��������: ��� ������� ���������� ����� �����-���� ����������
    -- ������. ��� ���� �� �������, ��� ���������� ������ � ������� ������� ��� ���������� �
    -- ��������� �� ����. ����� �� ������ ������ ����������� � ��������, �� �� ������
    -- �� ��������� ������ ����������. ��� ����� ��� ������� � ����� ������� :)
    syncUI $ do
    (indicator, indType, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
    aProgressIndicatorState =: (indicator, indType, arcname, direction, b+toRational(add_b), bytes', total')

-- |��������� ����� ���������� ���������
uiDoneProgressIndicator = do
  uiSuspendProgressIndicator
  aProgressIndicatorState =: (NoIndicator, undefined, undefined, undefined, undefined, undefined, undefined)

-- |�������� ���������� ������� � �������� � �������� ���������� ���������
uiWithProgressIndicator command arcsize action = do
  uiStartProgressIndicator INDICATOR_PERCENTS command return (return arcsize)
  ensureCtrlBreak "uiDoneProgressIndicator" uiDoneProgressIndicator action

{-# NOINLINE uiUpdateProgressIndicator #-}

