{-# OPTIONS_GHC -XNondecreasingIndentation -XScopedTypeVariables #-}
----------------------------------------------------------------------------------------------------
---- ������ � �������������� �������.                                                           ----
---- ���������� ���������������� ����� -rr � ������� r.                                         ----
---- ��������� writeRecoveryBlocks ���������� � ����� ������ recovery info,                     ----
----   ����������� ��� �������������� ������� �������� � ������.                                ----
---- ��������� pretestArchive ���������, ��������� �� �����, ��������� recovery info            ----
----   �/��� �������� ���������� ������                                                         ----
---- ��������� runArchiveRecovery ��������������� �����, ��������� recovery info.               ----
----------------------------------------------------------------------------------------------------
module ArcRecover where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Utils
import Files
import Charsets          (linesCRLF)
import Errors
import ByteStream
import Compression
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessRead   (writeControlBlock)
import ArcCreate         (testArchive, writeSFX)

-- |������ recovery info, ������� �� ����� ������������
aREC_VERSIONS = words "0.36 0.39"

-- |������ recovery info, ������� �� ���������� � �����, ������� �� ���������� recovery sectors
aREC_VERSION 0 = "0.39"
aREC_VERSION _ = "0.36"

{-
recovery info ������������ � ����� ��������� �������:

1. ����� ������ ������� recovery ������� (����� ���� 512/1k/2k/4k/... ����)
   ���� ����� ����������� �� ������� ����� �������. ��� ������� �� ���
   �������������� CRC32, ����� ����������� � recovery info.
2. ������������ � ���� �������� N recovery ��������, � ������ ������ ������
   (� ������� i) ������������ �� (i `mod` N)-�� ������ recovery. ��� ������� ������,
   ����������� �� ���� recovery ������, xor'���� ����� �����, � � recovery info
   ������������ �������������� ������. ����� �������, recovery info �������� �
   ���� N recovery ��������, ������ �� ������� �������� "����������" ���������� �
   ��������������� ��� �������� ������.

�������� ����������� ������ �������� � �������� CRC �������� ������. CRC,
�������� �� ������������� (������������ � recovery info), ��������, ��� ����
������ �������� ����.

�������������� ������ ��������, ���� �� ���� recovery ������ ���������� ��
����� ������ �������� ������� ������. � ���� ������ ���������� ����������
�������� ������� ����������� xor'����� ����������� recovery ������� � ����
��������� �������� ������, ��������������� ����� recovery �������.
-}

----------------------------------------------------------------------------------------------------
---- ������ recovery ���������� � ����� ������ -----------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������ ������ RECOVERY ������� � RAR (������������ ������ ��� ������� ����� -rr � RAR-����������� ����)
aRAR_REC_SECTOR_SIZE = 512

-- |�������� � ����� ���� RECOVERY
writeRecoveryBlocks archive oldarc init_pos command params bufOps = do
  rrPos <- archiveGetPos archive   -- ������� ������ recovery info � ������
  let -- ������ ������ � 1% �� ����
      arcsize      = rrPos - init_pos
      arcsize_1p   = arcsize `divRoundUp` 100
      -- ������������ �� ��������� ����� recovery info ������� �� ������� ������
      recommended_rr = if arcsize<3*10^5 then "4%" else
                       if arcsize<2*10^6 then "2%"
                                         else "1%"
      -- ������ ��������� ������ recovery info, ����������� � ����� ������
      old_recovery = ftRecovery (arcFooter oldarc)
      -- ����� ��������� ������ recovery info, ������������ ������ -rr � ������ ���������
      recovery = case opt_recovery command of
                   "-"     -> ""                                -- -rr-: ��������� ���������� � ������ recovery info
                   "--"    -> old_recovery                      -- �� ���������: ������������ ������� ��������� �����, ��������� � ��� �����
                   ""      -> old_recovery ||| recommended_rr   -- -rr: ������������ ������� ��������� ��� ������������� �����, ���� ������ recovery info � ����� �� �����������
                   "+"     -> old_recovery ||| recommended_rr   -- -rr+: �� �� �����
                   "0.1%"  -> "0*4096"                          -- -rr0.1%: ����������� ������ RR ��� �������������� ������ ����� internet
                   "0.01%" -> "0*65536"                         -- -rr0.01%: ��� ������� RR
                   r       -> r                                 -- -rr...: �������� � ����� ��������� ����� recovery info
  -- ����� �����, ���� ����� ��������, ��� ������� recovery info ��������� � �� �����
  if recovery==""  then return ([],"")  else do
      -- ���������� ������ ����� -rr � ���� recovery_amount;sector_size ��� rec_sectors*sector_size,
      -- ��������� ������ recovery ������� �/��� �� ����������, ���� ��� �������� ������ ����
  let (recovery_amount, explicit_rec_size, explicit_sector_size) = case () of
        _ | ';' `elem` recovery -> let (r,ss)  = split2 ';' recovery .$ mapSnd (i.parseSize) in
                                   (r,  Nothing,       Just ss)
          | '*' `elem` recovery -> let (ns,ss) = split2 '*' recovery .$ mapFstSnd (i.parseSize) in
                                   ("", Just (ns*ss),  Just ss)
          | otherwise           -> (recovery, Nothing, Nothing)
      -- ����������� ������ recovery info � �����
      wanted_rec_size = (case parseNumber recovery_amount 's' of
                             (num,'b') -> num                        -- ��� ����� � ������
                             (num,'s') -> num*aRAR_REC_SECTOR_SIZE   -- ����� � �������� �� 512 ����
                             (num,'%') -> arcsize_1p * num           -- ����� � ���������
                             (num,'p') -> arcsize_1p * num           -- -.-
                        -- ... �� ������ ���� �� ������ �������� ������ ��� 8-)
                        ) `minI` (getPhysicalMemory `div` 2)
      -- ������ recovery ������� ������� ����, ������� ��������� �� ������� ������ ��������
      -- recovery info - ��� ��� ������, ��� ������� ������ ������� ����� �������,
      -- �� ��������, ��� crc �������� ������ ����� �������� ������� ������� ����� recovery info.
      -- ���������� ������� recovery ������� ����������� ���������� ��������, �� �������
      -- ����������� �����, � ������������� ��������� ����������� �� ����������� �� �����
      -- recovery �������, �� ���� ����������� ����� �� �������������� ������.
      -- ��� ��������� ������������� ������ recovery info (� ���������, ��� ������� ������� ������),
      -- ������ recovery �������, ��������, ����� �� �������������.
      -- ����������� "����� recovery info -> ������ �������" ���������: 4% -> 512, 2% -> 1024, 1% -> 2048...
      sector_size =  explicit_sector_size `defaultVal`
                     case wanted_rec_size of
                       0 -> 4096  -- ��� ������� -rr0% ������������ ������ CRC 4-����������� ��������, ��� �������� 0.1% �� ������� ������
                       _ -> (2^lb(40*arcsize `div` wanted_rec_size)) `atLeast` 512
      -- ������ ��� ������������ ����� ������ � ��������
      arc_sectors = arcsize `divRoundUp` sector_size
      -- ������� ���� ����� ������ CRC ���� ��������
      crcs_size0  = arc_sectors * i (sizeOf (undefined::CRC))
      -- �������� ������ ����� recovery
      rec_size    = explicit_rec_size `defaultVal`
                    max wanted_rec_size (i crcs_size0+0*sector_size)  -- ���� recovery ������ ������� ��� ������� CRC �������� ������ ���� 0 recovery-��������
      -- ���������� recovery �������� � �� ����� �����
      rec_sectors = (rec_size - i crcs_size0) `divRoundUp` sector_size
      rec_sectors_size = rec_sectors*sector_size
      -- ������������� ������ ������ CRC, ���������� CRC ����� recovery ��������
      crcs_size   = crcs_size0 + rec_sectors * i (sizeOf (undefined::CRC))

  -- ��� ��������� ����������, ������ - �������� ������
  condPrintLineLn "r"$ "Protecting archive with "++show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++")..."
  uiStage              "0386 Protecting archive from damages"
  withPool $ \pool -> do
  sectors    <- pooledMallocBytes pool (i rec_sectors_size);   memset sectors 0 (i rec_sectors_size)
  buf        <- pooledMallocBytes pool (i sector_size)
  crcbuf     <- pooledMallocBytes pool (i (crcs_size+1))
  crc_stream <- ByteStream.createMemBuf crcbuf (i (crcs_size+1))
  -- �������� i �� � ���� ��� ����, ����� ��������� ������ � ������ ����������� �� ��������� ������
  -- � recovery info (��� ��������� ������������� �������������� ������ ��� ���� � ����� rec_sectors
  -- ���������������� ��������, ������� ����� ������������������ ��������, ������������
  -- �� ����� ������ � ������ recovery info ������)
  i' <- ref ((-arc_sectors) `mod` rec_sectors)
  -- ���� �� �������� ��� ���������� ����� ������, ����������� CRC ������� �� ���,
  -- � xor-���� ������ ������ � ��������������� ��� ������ recovery info
  archiveSeek archive init_pos
  uiWithProgressIndicator command arcsize $ do
    doChunks arcsize sector_size $ \bytes -> do
      uiUpdateProgressIndicator bytes
      failOnTerminated
      len <- archiveReadBuf archive buf bytes
      crc <- calcCRC buf bytes
      ByteStream.write crc_stream crc
      when (rec_sectors>0) $ do
        i <- val i';  i' =: (i+1) `mod` rec_sectors
        memxor (sectors +: i*sector_size) buf bytes
  -- �������� CRC ����� recovery ��������
  for [0..rec_sectors-1] $ \i -> do
    crc <- calcCRC (sectors +: i*sector_size) sector_size
    ByteStream.write crc_stream crc
  -- �������� ��� ��������� ����� recovery - � xor-��������� � � CRC ������ ������.
  -- �� ������ ���� ����� ���������� ��������� ����������, ����������� ��������� recovery info
  -- (����� ������, ������ � ����� ������ � ������ ���������� ����������,
  --  ���������� �������� � ������ ������� � ������ "���������", �� ������� ������� recovery info).
  -- ��� ���� ��� �������������� ������ ���������� ����������� ������ �������, �������� �����
  -- (��������� recovery ������� �� ������� ����� ������ ���������� �� ����� ��������������,
  --  ��������� CRC �������� ������, "���������������" � �� ��������, ������ ����� ������������).
  archiveSeek archive rrPos
  r0 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Nothing::Maybe Int) sectors (i rec_sectors_size) bufOps
  curpos <- archiveGetPos archive
  let addinfo = (aREC_VERSION rec_sectors, arcsize::Integer, curpos-init_pos::Integer, [(toInteger sector_size, toInteger rec_sectors)])
  r1 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Just addinfo) crcbuf (i crcs_size) bufOps
  return ([r0,r1],recovery)


-- |������ ��������� recovery info, ������������ ��� � ���������
readControlInfo crc_stream crcs_block = do
  ByteStream.rewindMemory crc_stream
  -- ������ recovery ���� ������ CRC �������� ������ ����� �������� ����-����������.
  -- ��� ������ �� � ������������ ������������ ��� ���������� � ������ ������ ���������,
  -- ����������� � ���� �������� ����-����������
  version <- ByteStream.read crc_stream
  if version `notElem` aREC_VERSIONS  then return$ Left version  else do
  -- ��������� ��������� ������� recovery �����, ���������� ��� ����������� ������
  -- �� ���� recovery ���������� - ��������� ����� ���������� ������ (����������� ���
  -- �������� (offset) �� ������ ������� recovery ����� �� ������ ���������� ������),
  -- ������ ���������� ������ (arcsize), � ������� ������ � ���������� recovery ��������
  -- � ������ "���������" recovery ����������
  (arcsize::Integer, offset::Integer) <- ByteStream.read crc_stream
  let init_pos = blPos crcs_block - offset
  (sector_size,rec_sectors):_ <- ByteStream.read crc_stream >>== mapFsts fromInteger >>== mapSnds fromInteger
  return$ Right (init_pos, arcsize, sector_size, rec_sectors)


----------------------------------------------------------------------------------------------------
---- �������� ������ ��� ������ recovery ���������� ------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ����������� ������, ����������� recovery ����������,
-- � ��������� �����, ���� ����� �������� ����
pretestArchive command archive footer = do
  when (opt_pretest command>0) $ do
    result <- withPool$ scanArchive command archive footer False
    case result of
      Just (_, sector_size, bad_crcs)  |  bad_sectors <- genericLength bad_crcs, bad_sectors>0
              -> registerError$ BROKEN_ARCHIVE (archiveName archive) ["0352 found %1 errors (%2)", show3 bad_sectors, showMemory (bad_sectors*sector_size)]
      Just _  -> condPrintLineLn "r" "Archive integrity OK"
      _       -> return ()
    -- ������ ������������ ������ ������ ��� -pt3 ��� ��� -pt2 � ���������� recovery ���������� � ������
    when (opt_pretest command==3 || (opt_pretest command==2 && isNothing result)) $ do
      w <- count_warnings $ do
               testArchive command (cmd_arcname command) doNothing3
      -- ���������� ������ ������ ��� ���������� warning'��
      when (w>0) $ do
        registerError$ BROKEN_ARCHIVE (archiveName archive) ["0353 there were %1 warnings due archive testing", show w]


-- |�������������� ����� � ���������� ������ ������� ��������
-- (������������ ���������� CRC ������� ������� � ��� CRC, ����������� �� ������ recovery �����)
scanArchive command archive footer recovery pool = do
  -- ����� recovery ����� � ������. �������� ������ ����� ���������� ������ ���� ���� recovery ������
  let recovery_blocks  =  filter ((RECOVERY_BLOCK==).blType) (ftBlocks footer)
  if (length recovery_blocks < 2)  then return Nothing  else do
  let sectors_block:crcs_block:_ = recovery_blocks
  when (length recovery_blocks > 2) $ do
      registerWarning$ GENERAL_ERROR ["0344 only first of %1 recovery records can be processed by this program version. Please use newer versions to process the rest", show (length recovery_blocks `div` 2)]

  -- ��������� RECOVERY ����� (�������+crcs)
  sectors <- if recovery  then archiveBlockReadUnchecked pool sectors_block
                          else return$ error "scanArchive:sectors undefined"
  (crcbuf, crcsize) <- archiveBlockReadAll pool (error "encrypted recovery block") crcs_block
  crc_stream <- ByteStream.openMemory crcbuf crcsize

  -- ��������� ��������� crc_stream, ���������� ��� ����������� ������ �� ���� recovery ����������
  info <- readControlInfo crc_stream crcs_block
  case info of
    Left version -> do registerWarning$ GENERAL_ERROR ["0345 you need FreeArc %1 or above to process this recovery info", version]
                       return Nothing
    Right (init_pos, arcsize, sector_size, rec_sectors) -> do
      -- ��-xor-���� ������� ������ � ���������������� ��������� RECOVERY �����.
      -- ������� � bad_crcs ������ �������� ������, ��� CRC �� ��������� � ������������.
      condPrintLineLn "r"$ show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++") present"
      condPrintLineLn "r"$ "Scanning archive for damages..."
      uiStage              "0385 Scanning archive for damages"
      archiveSeek archive init_pos
      buf <- pooledMallocBytes pool sector_size
      -- ������ ���������� ����� ������ � ��������
      let arc_sectors = i$ arcsize `divRoundUp` sector_size
      -- i ���������� �� � ���� ������ ��� (��. � writeRecoveryBlocks)
      i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
      bad_crcs <- withList $ \bad_crcs -> do
        -- ���� �� �������� ������ � ������������ ���������� ���������
        uiWithProgressIndicator command arcsize $ do
          doChunks arcsize sector_size $ \bytes -> do
            uiUpdateProgressIndicator bytes
            failOnTerminated
            len <- archiveReadBuf archive buf bytes
            -- Xor'�� �������, ��������������� ������ recovery �������, ����� �������� ������ ��� �������������� �������� �������
            when (recovery && rec_sectors>0) $ do
              i <- val i';  i' =: (i+1) `mod` rec_sectors
              memxor (sectors +: i*sector_size) buf bytes
            -- ��������� ������ ������� �������� (��� CRC �� ��������� � �����������)
            n <- val n';  n `seq` (n' =: n+1)
            crc          <- calcCRC buf bytes
            original_crc <- ByteStream.read crc_stream
            when (crc/=original_crc) $ do
              bad_crcs <<= n
      return$ Just ((crcs_block,crc_stream,sectors,buf), sector_size, bad_crcs)


----------------------------------------------------------------------------------------------------
---- �������������� ������ � ������� recovery ���������� -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� �������������� ������������ ������
runArchiveRecovery command@Command{ cmd_filespecs       = filespecs
                                  , cmd_arcname         = arcname
                                  , opt_original        = opt_original
                                  , opt_save_bad_ranges = opt_save_bad_ranges
                                  } = do
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []
  let arcname_fixed = arcname `replaceBaseName` ("fixed."++takeBaseName arcname)
  whenM (fileExist arcname_fixed) $ do
    registerError$ GENERAL_ERROR ["0346 file %1 already exists", arcname_fixed]
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  withPool $ \pool -> do   -- ���������� ��� ������, ����� ������������� ���������� ���������� ������ ��� ������
  bracketCtrlBreak "archiveClose1:ArcRecover" (archiveReadFooter command arcname) (archiveClose.fst) $ \(archive,footer) -> do
    -- ������ ���� - ������������ ������ � ����������� ������ ������� ��������
    result <- scanArchive command archive footer True pool
    if isNothing result
        then registerError$ GENERAL_ERROR ["0347 archive can't be recovered - recovery data absent or corrupt"]
        else do
    -- ��������� � �������������� ������
    let Just ((crcs_block,crc_stream,sectors,buf),_,bad_crcs) = result
    if null bad_crcs  then condPrintLine "n"$ "Archive ok, no need to restore it!"  else do
    -- ��������� ��������� crc_stream, ���������� ��� ����������� ������ �� ���� recovery ����������
    Right (init_pos, arcsize, sector_size, rec_sectors) <- readControlInfo crc_stream crcs_block

    -- ��������� ������ ��������, ������� �� ������ ������������, � ���, ������� ����������
    -- �� ���� � ��� �� recovery ������ � ������ �� ����� ���� �������������
    let (recoverable,bad)  =  case rec_sectors of
           0 -> ([], bad_crcs)      -- ���� RR �� �������� recovery sectors, �� �� ���� ������ ������ �� ����� ���� ������������ � �� ������� :D
           _ -> bad_crcs .$ sort_and_groupOn (`mod` rec_sectors)   -- ������������� ������ �� ������� �������, ������� ���������� �� ���� ������ RECOVERY
                         .$ partition (null.tail)                  -- �������� ������, ��� ������ ���� ������� (������, ������� ������� ���������� ������������), �� ������
                         .$ mapFst concat .$ mapSnd concat
        bad_sectors = genericLength bad
        recoverable_sectors = genericLength recoverable

    -- ��� ��������� ���������� � ���� ������ ����������� ���������� ���� � ������
    let arcPos sector = sector*sector_size+init_pos
    let save_bad_ranges bad_sectors = do
          when (opt_save_bad_ranges>"") $ do
            let byte_range sector = show start++"-"++show end
                                      where start = arcPos sector
                                            end   = start+sector_size-1
            filePutBinary opt_save_bad_ranges (joinWith "," $ map byte_range bad_sectors)

    -- ���� �� ������ �� ����� ������������ - ��� ������� ������ ������������ :)
    originalName <- originalURL opt_original arcname
    when (recoverable==[] && originalName=="") $ do
      save_bad_ranges bad
      registerError$ GENERAL_ERROR ["0348 %1 unrecoverable errors (%2) found, can't restore anything!",
                                    show3 bad_sectors, showMemory (bad_sectors*sector_size)]

    -- ��������� ����, ���������� ���������� ���������� ������ ������� �������� �� ������ recoverable (bad ������� ������������ ���������� ��-�� ���������� �������������)
    condPrintLineLn "n"$ show3 recoverable_sectors++" recoverable errors ("++showMemory (recoverable_sectors*sector_size)++") "
                         ++(bad &&& "and "++show3 bad_sectors++" unrecoverable errors ("++showMemory (bad_sectors*sector_size)++") ")
                         ++"found"
    archiveFullSize <- archiveGetSize archive
    condPrintLineLn "n"$ "Recovering "++showMem archiveFullSize++" archive..."
    uiStage              "0387 Recovering archive"
    errors' <- ref bad
    -- ��������� � �������� ������ � ���������������� �������
    handleCtrlBreak  "fileRemove arcname_fixed" (ignoreErrors$ fileRemove arcname_fixed) $ do
    bracketCtrlBreak "archiveClose2:ArcRecover" (archiveCreateRW arcname_fixed) (archiveClose) $ \new_archive -> do
    withJIT (fileOpen =<< originalURL originalName arcname) fileClose $ \original' -> do   -- ������ ������� ����, ������ ����� ��������� ���������� ������
    writeSFX (opt_sfx command) new_archive (dirlessArchive archive footer)   -- ������ �������� ������ � ������ SFX-������
    archiveSeek archive init_pos
    -- ������ ���������� ����� ������ � ��������
    let arc_sectors = i$ arcsize `divRoundUp` sector_size
    -- i ���������� �� � ���� ������ ��� (��. � writeRecoveryBlocks)
    i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
    originalErr <- init_once

    -- ���� �� �������� ������������������ ������ � ������������ ���������� ���������
    uiWithProgressIndicator command arcsize $ do
      doChunks arcsize sector_size $ \bytes -> do
        uiUpdateProgressIndicator bytes
        failOnTerminated
        i <- val i';  when (rec_sectors>0) $  do i `seq` (i' =: (i+1) `mod` rec_sectors)
        n <- val n';  n' =: n+1
        len <- archiveReadBuf archive buf bytes
        original_crc <- ByteStream.read crc_stream

        -- ���� ��� ���� �� ����������������� ��������, �� ����������� ��� ����������,
        -- �������� ��� � ����������� ��������, ������� ������ �������� ��� ���
        -- ����������� ��� �������������� ������
        when (n `elem` recoverable) $ do
          let do_xor = memxor buf (sectors +: i*sector_size) bytes
          do_xor
          -- ���� CRC � ����� ����� �� ������� (��� �������� ��� ������ � ����� ����������� �������),
          -- �� ����������� �������� ���������� ������� � ��������,
          -- ��� � ������ �������� ����������������� �������
          crc <- calcCRC buf bytes
          when (crc/=original_crc) $ do
            do_xor;  errors' .= (n:)

        -- ���� ��� ������� ������, ��������������� � ������� ��������� ����������,
        -- �� ������ �������� ��� ������ (���� ������� --original)
        errors <- val errors'
        when (originalName>"" && n `elem` errors) $ do
          -- ������ ����� ��������, ��� original-���� ������� �������
          eitherM_ (try $ valJIT original')
            ( \(_ :: SomeException) -> once originalErr$ registerWarning$ GENERAL_ERROR ["0349 can't open original at %1", originalName])
            $ \original  -> do
          -- ������ ��������, ��� ��� ������ ��������� � ����������������� �������
          dwnl_size <- fileGetSize original
          if dwnl_size /= archiveFullSize
            then once originalErr$ registerWarning$ GENERAL_ERROR
                      ["0350 %1 has size %2 so it can't be used to recover %3 having size %4",
                       originalName, show3 dwnl_size, arcname, show3 archiveFullSize]
            else do
          -- ������ �� original ������� ������
          allocaBytes bytes $ \temp -> do
          fileSeek    original (arcPos n)
          fileReadBuf original temp bytes
          -- ���� ����������� ������ ����� ������ CRC - ������� �� ������, ����������� �� ��������� ������
          crc <- calcCRC temp bytes
          when (crc==original_crc) $ do
            copyBytes buf temp bytes
            errors' .= delete n

        -- �������� [���������������] ������ � ����� �����
        archiveWriteBuf new_archive buf bytes

    -- ����������� ����� recovery (� ������, ������ ���� ������� ������� ��������� ����� ����� ���������� ������)
    pos <- archiveGetPos archive
    archiveCopyData archive pos (archiveFullSize-pos) new_archive

    condPrintLineLn "n"$ "Recovered archive saved to "++arcname_fixed
    errors <- val errors'
    save_bad_ranges errors
    when (errors>[]) $ do
      let errnum = genericLength errors
      registerWarning$ GENERAL_ERROR ["0351 %1 errors (%2) remain unrecovered", show3 errnum, showMemory (errnum*sector_size)]
  return (1,0,0,0)



-- |��������� URL ���������, ������ �� ����������� ����� --original � ����� ������
originalURL opt_original arcname =
  case opt_original of
    "--"         -> return ""              -- ���������
    '?':command  -> run_command command    -- URL ������������ ����������� ������� `command arcname`
    ""           -> auto_url               -- URL ������������ ������������� �� files.bbs/descript.ion
    url          -> return url             -- URL ������ ����
 where

  -- ��������� ������� � ������� � ����� � �������� URL
  run_command command  =  runProgram (command++" "++arcname)
                          >>== head.linesCRLF

  -- ������������� ����������� URL �� �������� ������ � files.bbs/descript.ion
  auto_url = mapMaybeM try_descr (words "files.bbs descript.ion") >>== catMaybes >>== listToMaybe >>== fromMaybe ""

  -- �������� URL ������ � ����� �������� descr
  try_descr descr = do
    let descrname = takeDirectory arcname </> descr
        basename  = takeFileName  arcname
    fileExist descrname >>= bool (return Nothing) (do
    fileGetBinary descrname >>== linesCRLF
      -- ������, ������������ � ��������, ���� ���������� � ���������� (��� ������ ����������� ��������)
      >>== joinContLines ""
      -- ������� ������ � files.bbs ����� ���������� � name.arc ��� � "The Name.arc", �� ������� ��� ������
      >>== listToMaybe . filter (isSpace.head)
           . catMaybes . concatMap (\x -> [x.$startFrom basename
                                          ,x.$startFrom ("\""++basename++"\"")])
      -- ������� URL �� ������ � ���������
      >>== fmap findURL
    )

  findURL s = firstJust$ map getURL$ strPositions s "://"
    where
      -- �������� �� ������ s URL, ��� "://" ��������� �� �������� n
      getURL n = let (pre,post) = splitAt n s
                     prefix  = reverse$ takeWhile isURLPrefix$ reverse pre
                     postfix = takeWhile isURLChar$ drop 3 post
                 in
                     prefix &&& postfix &&& Just (prefix++"://"++postfix)

  -- �������, ������� ����� ����������� � �������� ��� ���� URL
  isURLPrefix = anyf [isAsciiLower, isAsciiUpper]
  isURLChar   = anyf [flip elem "+-=._/*(),@'$:;&!?%", isDigit, isAsciiLower, isAsciiUpper]

  -- ����� ������ ����������� (������������ � ��������) � ����������� ��������
  joinContLines prev (x@(c:_):xs) | isSpace c   =   joinContLines (prev++x) xs
  joinContLines prev (x:xs)                     =   prev : joinContLines x xs
  joinContLines prev []                         =   [prev]

