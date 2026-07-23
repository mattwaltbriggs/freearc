{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- �������������� ������������ � ���� ���������� ��������� (CUI - Console User Interface).  ------
----------------------------------------------------------------------------------------------------
module UIBase where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char
import Data.IORef
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.IO.Unsafe
import System.Time
#ifdef FREEARC_UNIX
import System.Posix.IO
import System.Posix.Terminal
#endif

import Utils
import Errors
import Files
import FileInfo
import Options


-- |����� �������� ��� ���������� � ������� � �������� � ����������, ��������� ��� �����������
-- ���������� ��������� � ������ ��������� ����������
data UI_State = UI_State {
    total_files     :: !FileCount   -- ���-�� ������, ������� ��� ������ ����������
  , total_bytes     :: !FileSize    -- ����� ����� ���� ������ (� ������������� ����)
  , archive_total_bytes      :: !FileSize    -- ����� ����� ������ � ������ - ��������������� ������ ��� ������ ����������
  , archive_total_compressed :: !FileSize    -- ����� ����� ������ � ������ (� ������ ����)
  , datatype        ::  DataType    -- �������������� � ������ ������ ����� ������: ����/�������/��������� ������
  , uiFileinfo      :: !(Maybe FileInfo)  -- ������� �������������� ���� (���� ����)
  -- � ����������� �� ����, ����� ����� ������ ������ ��������������, ���������� ���������
  -- ���� �� ���� ������:
  ,    files        :: !FileCount   -- ���-�� ��� ������������ ������
  ,    bytes        :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    cbytes       :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ���� �� ���� ��������� (��������� ���������� �� ��������������):
  ,    dirs         :: !FileCount   -- ���-�� ��������� ��������� � ������ ��������� ������
  ,    dir_bytes    :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    dir_cbytes   :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ����� ����, �� ����������, ����� ����� �� ���� ������ - �� ����� ���� �� ������������� (��� ������� ��� ����������� �������� �������� ��������):
  ,    fake_files   :: !FileCount   -- ���-�� ��� ������������ ������
  ,    fake_bytes   :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    fake_cbytes  :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ���������� � ������� �����-�����
  ,    algorithmsCount :: Int       -- ���-�� ���������� � �������
  ,    rw_ops       :: [[UI_RW FileSize]] -- ������������������ �������� ������/������ � ��������� �� ��������� ����������
  ,    r_bytes      :: FileSize     -- ����� ��� ������������ ������ �� ����� ������� ��������� ������
  ,    rnum_bytes   :: FileSize     -- ����� ��� ������������ ������ �� ����� ���������� ��������� ������
  }

-- |�������������� � ������ ������ ����� ������: ����/�������/��������� ������
data DataType = File | Dir | CData   deriving Eq

-- |�������� ������ � ������ � ������ ��������
data UI_RW a = UI_Read a | UI_Write a

-- |��� ���������� - ������ ������� ��� + �����/...
data IndicatorType = INDICATOR_PERCENTS | INDICATOR_FULL   deriving Eq


-- ����������� ������ �������
ref_command               =  unsafePerformIO$ newIORef$ error "undefined UI::ref_command"
-- �������������� ����� (�� ��������� � command.$cmd_arcname ��� ������������ ���������� ������ ����� ��������)
uiArcname                 =  unsafePerformIO$ newIORef$ error "undefined UI::uiArcname"
refStartArchiveTime       =  unsafePerformIO$ newIORef$ error "undefined UI::refStartArchiveTime"
refStartPauseTime         =  unsafePerformIO$ newIORef$ error "undefined UI::refStartPauseTime"
refArchiveProcessingTime  =  unsafePerformIO$ newIORef$ error "undefined UI::refArchiveProcessingTime"  :: IORef Double
ref_ui_state              =  unsafePerformIO$ newIORef$ error "undefined UI::ref_ui_state"
putHeader                 =  unsafePerformIO$ init_once
ref_w0                    =  unsafePerformIO$ newIORef$ error "undefined UI::ref_w0"         :: IORef Int
ref_arcExist              =  unsafePerformIO$ newIORef$ error "undefined UI::ref_arcExist"   :: IORef Bool
-- ������� ������ ���������� ������� ��� ��� ����� �� uiFileinfo
uiMessage                 =  unsafePerformIO$ newIORef$ ""
-- |������� ���������������� ������
files_scanned             =  unsafePerformIO$ newIORef$ (0::Integer)

-- |���������� ����������, �������� ��������� ���������� ���������
aProgressIndicatorState    =  unsafePerformIO$ newIORef$ error "undefined UI::aProgressIndicatorState"
aProgressIndicatorEnabled  =  unsafePerformIO$ newIORef$ False
-- |����� ������ ������� �������� ���������
indicator_start_real_secs  =  unsafePerformIO$ newIORef$ (0::Double)

-- |������������� ������� � UI
syncUI = withMVar mvarSyncUI . const;  mvarSyncUI = unsafePerformIO$ newMVar "mvarSyncUI"

{-# NOINLINE indicators #-}
-- |���������� ��� ������������ ������ ���������
indicators  = unsafePerformIO$ newMVar$ ([]::[MVar Message])   -- list of indicator threads
type Message = (Update, IO())                                  -- message sent to indicator thread in order to make an update
data Update  = ForceUpdate | LazyUpdate  deriving (Eq)         -- ForceUpdate message requesting whole update sent after (de)compression has been finished

-- |������������� �������� ��� ����������
updateAllIndicators = do
  indicators' <- val indicators
  for indicators' $ \indicator -> do
    x <- newEmptyMVar
    putMVar indicator (ForceUpdate, putMVar x ())
    takeMVar x

-- |��������� � ���������� action ������ secs ������
backgroundThread secs action = do
  x <- newEmptyMVar
  indicators ++= [x]  -- ���������� � ���� ������ ��������� ����� ��������� ���������� "�����"
  forkIO $ do
    foreverM $ do
      sleepSeconds secs
      putMVar x (LazyUpdate, doNothing0)
  forkIO $ do
    foreverM $ do
      (updateMode, afterAction) <- takeMVar x
      syncUI $ do
        action updateMode
      afterAction

-- |����, �������� �� indicator, � ��������� ����� �� ������� ��� ���������� ��������
indicatorThread secs output =
  backgroundThread secs $ \updateMode -> do
    whenM (val aProgressIndicatorEnabled) $ do
      operationTerminated' <- val operationTerminated
      (indicator, indType, arcname, direction, bRational :: Rational, bytes', total') <- val aProgressIndicatorState
      let b = round bRational  -- we use Rational in order to save decimal fractions (results of 90%/10% counting rule)
      when (indicator /= NoIndicator  &&  not operationTerminated') $ do
        bytes <- bytes' b;  total <- total'
        -- ��������� ������ ������������ ������ � ������ ������
        let processed = total>0 &&& (fromIntegral bytes / fromIntegral total :: Double)
        secs <- return_real_secs
        sec0 <- val indicator_start_real_secs
        let remains  = if processed>0.001  then " "++showHMS(sec0+(secs-sec0)/processed-secs)  else ""
            winTitle = "{"++trimLeft p++remains++"}" ++ direction ++ takeFileName arcname
            p        = percents indicator bytes total
        output updateMode indicator indType winTitle b bytes total processed p

{-# NOINLINE updateAllIndicators #-}
{-# NOINLINE backgroundThread #-}
{-# NOINLINE indicatorThread  #-}


----------------------------------------------------------------------------------------------------
---- ��������� ��������� ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���� ���������� ��������� (����������, ��������, ������� ��������)
data Indicator = NoIndicator | ShortIndicator | LongIndicator   deriving (Eq)

bytes_per_sec = 1*mb  -- Typical (de)compression speed

-- |������� ��������� ���������, ����������� �� ���������� ���������� :)
select_indicator command total_bytes  =  case (opt_indicator command)
  of "0"                                    ->  NoIndicator      -- ����� "-i" - ��������� ���������!
     _ | i total_bytes < bytes_per_sec*100  ->  ShortIndicator   -- ��������� � ���������, ���� ����� ����� ������ ������ 100 �� (��� ���� � ������� �������������� ������ ������ �������� ������)
       | otherwise                          ->  LongIndicator    -- ��������� � ������� ����� ��������, ���� ������ ������ 100 ��

-- |������� ��������� ��������� � ������������ � ��������� ���������
percents NoIndicator    current total  =  ""
percents ShortIndicator current total  =  right_justify 3 (ratio2 current total) ++ "%"
percents LongIndicator  current total  =  right_justify 5 (ratio3 current total) ++ "%"

-- |������� ����� ��� ���������� ���������
open_percents     =  flip replicate ' '  . indicator_len
-- |��������� ����� �� ������� ��������, ������� �������� ��������� ���������
back_percents     =  flip replicate '\b' . indicator_len
-- |���������� ������� ������ ����������������� ���������� ���������
clear_percents i  =  back_percents i ++ open_percents i

-- |������ ���������� ��������� � ��������
indicator_len NoIndicator    = 0
indicator_len ShortIndicator = 4
indicator_len LongIndicator  = 6

-- |Format percent ratio with 2 digits
ratio2 count 0     =  "0"
ratio2 count total =  show$ count*100 `div` total

-- |Format percent ratio with 2+1 digits
ratio3 count 0     =  "0.0"
ratio3 count total =  case (show$ count*1000 `div` total) of
                        [digit]  -> "0." ++ [digit]
                        digits   -> init digits ++ ['.', last digits]

-- |������� �����, ������� ������, �������� � �.�.: "1.234.567"
show3 :: (Show a) => a -> [Char]
show3 = reverse.xxx.reverse.show
          where xxx (a:b:c:d:e) = a:b:c:'.': xxx (d:e)
                xxx a = a

{-# NOINLINE ratio2 #-}
{-# NOINLINE ratio3 #-}
{-# NOINLINE show3 #-}


----------------------------------------------------------------------------------------------------
---- ��������������� ������� ��� �������������� �����/����� � ������ � �������� --------------------
----------------------------------------------------------------------------------------------------

-- |������� ����� ����� ��������� � �������� - ���������� ����������� ����������� �������������!!!
diffTimes (TOD sa pa) (TOD sb pb)  =  i(sa - sb) + (i(pa-pb) / 1e12)

-- |�������� ������� � �������
addTime :: ClockTime -> Double -> ClockTime
addTime (TOD sa pa) secs  = TOD (sa+sb+sc) pc
  where
    sb = floor secs :: Integer
    pb = round$ (secs - fromIntegral sb)*1e12
    (sc,pc) = (pa+pb) `divMod` (10^12)

-- |���������� ����� � ��������� ������ (������ ��� ����� � ������ �������)
getUnixTime = do
  (TOD seconds picoseconds) <- getClockTime
  return seconds

-- |���������� ����� �������� � ����������� ������, � ������� ������
show_ratio cmd bytes cbytes =
  ""        ++ show3       (if (cmdType cmd == ADD_CMD) then bytes else cbytes) ++
   " => "   ++ show_bytes3 (if (cmdType cmd == ADD_CMD) then cbytes else bytes) ++ ". " ++
   "Ratio " ++ ratio3 cbytes bytes ++ "%"

-- |���������� ������, ����������� �������� �����
showTime secs  =  showFFloat (Just 2) secs " secs"

-- |���������� ������, ����������� �������� ��������
showSpeed bytes secs  =  show3(round$ i bytes/1000/secs) ++ " kB/s"

-- |��������������� ����� ��� H:MM:SS
showHMS secs  =  show hour++":"++left_fill '0' 2 (show min)++":"++left_fill '0' 2 (show sec)
  where
    s = round secs
    sec = (s `mod` 60)
    min = (s `div` 60) `mod` 60
    hour= (s `div` 3600)



-- |�������� �����, ����� ���� ���������� ����������� ����� ��������� (����� ��� ���������� ����������)
debugLog label = do
  condPrintLine   "$" $  label   -- �������� label � ���������� � ��������
  real_secs <- return_real_secs
  condPrintLineLn "$" $  ": " ++ showTime real_secs

-- |������� ���������� � ������, ���� �� �������� ��� ������� ��� ��������
debugLogList label list = do
  drop 1 list &&& debugLog (format label (show3$ length list))

-- |�������� ������� � ���������� ����� ���������
debugLog0 = condPrintLineLn "$"

-- |�����, ������� ��������� � ������ ���������� ������� ��� ������� �������
return_real_secs = do
  start_time    <- val refStartArchiveTime
  current_time  <- getClockTime
  return$ diffTimes current_time start_time

-- �������� �����, ���������� � �����, �� ��������� ������� ���������� �������
pause_real_secs = do
  refStartPauseTime =:: getClockTime

resume_real_secs = do
  start_time    <- val refStartPauseTime
  current_time  <- getClockTime
  let pause = diffTimes current_time start_time :: Double
  refStartArchiveTime .= (flip addTime pause)

pauseTiming = bracket_ pause_real_secs resume_real_secs

{-# NOINLINE diffTimes #-}
{-# NOINLINE show_ratio #-}
{-# NOINLINE debugLog #-}


----------------------------------------------------------------------------------------------------
---- ����� ���������, ��������������� ����������� ������� ------------------------------------------
----------------------------------------------------------------------------------------------------

msgStart cmd arcExist =
                case (cmdType cmd, arcExist) of
                  (ADD_CMD,     False)  ->  "Creating archive: "
                  (ADD_CMD,     True)   ->  "Updating archive: "
                  (LIST_CMD,    _)      ->  "Listing archive: "
                  (TEST_CMD,    _)      ->  "Testing archive: "
                  (EXTRACT_CMD, _)      ->  "Extracting archive: "
                  (RECOVER_CMD, _)      ->  "Recovering archive: "

msgStartGUI cmd arcExist =
                case (cmd, cmdType cmd, arcExist) of
                  ("ch", _,           _)      ->  "0433 Modifying archive %1"
                  ("j",  _,           _)      ->  "0240 Joining archives to %1"
                  ("d",  _,           _)      ->  "0435 Deleting files from archive %1"
                  ("k",  _,           _)      ->  "0300 Locking archive %1"
                  (_,    ADD_CMD,     False)  ->  "0437 Creating archive %1"
                  (_,    ADD_CMD,     True)   ->  "0438 Updating archive %1"
                  (_,    LIST_CMD,    _)      ->  "0439 Listing archive %1"
                  (_,    TEST_CMD,    _)      ->  "0440 Testing archive %1"
                  (_,    EXTRACT_CMD, _)      ->  "0441 Extracting files from archive %1"
                  (_,    RECOVER_CMD, _)      ->  "0382 Repairing archive %1"

msgFinishGUI cmd arcExist warnings@0 =
                case (cmd, cmdType cmd, arcExist) of
                  ("ch", _,           _)      ->  "0238 SUCCESFULLY MODIFIED %1"
                  ("j",  _,           _)      ->  "0241 SUCCESFULLY JOINED ARCHIVES TO %1"
                  ("d",  _,           _)      ->  "0229 FILES SUCCESFULLY DELETED FROM %1"
                  ("k",  _,           _)      ->  "0301 SUCCESFULLY LOCKED ARCHIVE %1"
                  (_,    ADD_CMD,     False)  ->  "0443 SUCCESFULLY CREATED %1"
                  (_,    ADD_CMD,     True)   ->  "0444 SUCCESFULLY UPDATED %1"
                  (_,    LIST_CMD,    _)      ->  "0445 SUCCESFULLY LISTED %1"
                  (_,    TEST_CMD,    _)      ->  "0232 SUCCESFULLY TESTED %1"
                  (_,    EXTRACT_CMD, _)      ->  "0235 FILES SUCCESFULLY EXTRACTED FROM %1"
                  (_,    RECOVER_CMD, _)      ->  "0383 SUCCESFULLY REPAIRED ARCHIVE %1"

msgFinishGUI cmd arcExist warnings =
                case (cmd, cmdType cmd, arcExist) of
                  ("ch", _,           _)      ->  "0239 %2 WARNINGS WHILE MODIFYING %1"
                  ("j",  _,           _)      ->  "0242 %2 WARNINGS WHILE JOINING ARCHIVES TO %1"
                  ("d",  _,           _)      ->  "0230 %2 WARNINGS WHILE DELETING FROM %1"
                  ("k",  _,           _)      ->  "0302 %2 WARNINGS WHILE LOCKING ARCHIVE %1"
                  (_,    ADD_CMD,     False)  ->  "0434 %2 WARNINGS WHILE CREATING %1"
                  (_,    ADD_CMD,     True)   ->  "0436 %2 WARNINGS WHILE UPDATING %1"
                  (_,    LIST_CMD,    _)      ->  "0442 %2 WARNINGS WHILE LISTING %1"
                  (_,    TEST_CMD,    _)      ->  "0233 %2 WARNINGS WHILE TESTING %1"
                  (_,    EXTRACT_CMD, _)      ->  "0236 %2 WARNINGS WHILE EXTRACTING FILES FROM %1"
                  (_,    RECOVER_CMD, _)      ->  "0384 %2 WARNINGS WHILE REPAIRING ARCHIVE %1"

msgDo cmd    =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressing "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extracting "

msgFile      =  ("  "++).msgDo

msgDone cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressed "
                  TEST_CMD    -> "Tested "
                  EXTRACT_CMD -> "Extracted "

msgStat cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compression "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extraction "

-- |���������� "file" ��� "files", � ����������� �� ���-��
show_files3 1 = "1 file"
show_files3 n = show3 n ++ " files"

-- |���������� "archive" ��� "archives", � ����������� �� ���-��
show_archives3 1 = "1 archive"
show_archives3 n = show3 n ++ " archives"

-- |���������� "byte" ��� "bytes", � ����������� �� ���-��
show_bytes3 1 = "1 byte"
show_bytes3 n = show3 n ++ " bytes"


{-
  ��������� UI:
  - ���� �������, ���������� ���������� �� ��������/���������� � ������������ ���������
      �������������� � UI:
        ui_PROCESS pipe = do
          (StartCommand cmd) <- receiveP pipe
            (StartArchive cmd) <- receiveP pipe
              (StartFile fi fi) <- receiveP pipe
                (UnpackedData n) <- receiveP pipe
                (CompressedData n) <- receiveP pipe
            (EndArchive) <- receiveP pipe
          (EndCommand) <- receiveP pipe
         (EndProgram) <- receiveP pipe
      ���� ������� ���������� ������� ��������� UI � SampleVar
-}
