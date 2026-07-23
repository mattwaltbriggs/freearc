{-# OPTIONS_GHC -XNondecreasingIndentation -XRecursiveDo -XNoMonomorphismRestriction #-}
----------------------------------------------------------------------------------------------------
---- ������� �������� ������ � ��������� ���������� ������, � ������ ����������� ������ � �����.----
---- ���������� �� ArcCreate.hs                                                                 ----
----------------------------------------------------------------------------------------------------
module ArcvProcessCompress where

import Prelude hiding (catch)
import Control.Monad
import Data.IORef
import Data.Array.IO
import Foreign.C.Types
import Foreign.Ptr

import Utils
import Files
import Errors
import Process
import FileInfo
import Compression
import Encryption
import Options           (opt_data_password, opt_headers_password, opt_encryption_algorithm, limit_compression)
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessExtract
import ArcvProcessRead


-- |������� �������� ������ � ��������� ���������� ������, � ������ ����������� ������ � �����.
-- ����� ���������� ����� backdoor ��������� ���������� � ������, ��������� ��� ������ ������
compress_AND_write_to_archive_PROCESS archive command backdoor pipe = do

  -- ��������� ����������� � UI ������� ������
  let display (FileStart fi)               =  uiStartFile      fi
      display (DataChunk buf len)          =  uiUnpackedBytes  (i len)
      display (CorrectTotals files bytes)  =  uiCorrectTotal   files bytes
      display (FakeFiles cfiles)           =  uiFakeFiles      (map cfFileInfo cfiles) 0
      display _                            =  return ()

  -- ��������� ������ ����������� ������ � �����
  let write_to_archive (DataBuf buf len) =  do uiCompressedBytes  (i len)
                                               archiveWriteBuf    archive buf len
                                               return len
      write_to_archive  NoMoreData       =  return 0

  -- ��������� ����������� ������� �����-����� �� �������� ������ � �������� ��� ������������
  let copy_block = do
        CopySolidBlock files <- receiveP pipe
        let block       = (cfArcBlock (head files))
        uiFakeFiles       (map cfFileInfo files)  (blCompSize block)
        archiveCopyData   (blArchive block) (blPos block) (blCompSize block) archive
        DataEnd <- receiveP pipe
        return ()

  repeat_while (receiveP pipe) (notTheEnd) $ \msg -> case msg of
    DebugLog str -> do   -- ���������� ���������� ���������
        debugLog str
    DebugLog0 str -> do
        debugLog0 str
    CompressData block_type compressor real_compressor just_copy -> mdo
        case block_type of             -- ������� UI ������ ���� ������ ������ ����� ����������
            DATA_BLOCK  ->  uiStartFiles (length real_compressor)
            DIR_BLOCK   ->  uiStartDirectory
            _           ->  uiStartControlData
        result <- ref 0   -- ���������� ����, ���������� � ��������� ������ write_to_archive

        -- ������� CRC (������ ��� ��������� ������) � ���������� ���� � ������������� ������ �����
        crc      <- ref aINIT_CRC
        origsize <- ref 0
        let update_crc (DataChunk buf len) =  do when (block_type/=DATA_BLOCK) $ do
                                                     crc .<- updateCRC buf len
                                                 origsize += i len
            update_crc _                   =  return ()

        -- �������, ����� �� ���������� ��� ����� �����
        let useEncryption = password>""
            password = case block_type of
                         DATA_BLOCK     -> opt_data_password command
                         DIR_BLOCK      -> opt_headers_password command
                         FOOTER_BLOCK   -> opt_headers_password command
                         DESCR_BLOCK    -> ""
                         HEADER_BLOCK   -> ""
                         RECOVERY_BLOCK -> ""
                         _              -> error$ "Unexpected block type "++show (fromEnum block_type)++" in compress_AND_write_to_archive_PROCESS"
            algorithm = command.$ opt_encryption_algorithm

        -- ���� ��� ����� ����� ����� ������������ ����������, �� �������� �������� ����������
        -- � ������� ������� ������. � ������� ���������� �������� ���������� ��������� key � initVector,
        -- � � ������ ������������ salt � checkCode, ����������� ��� ������� �������� ������
        (add_real_encryption, add_encryption_info) <- if useEncryption
                                                         then generateEncryption algorithm password   -- not thread-safe due to use of PRNG!
                                                         else return (id,id)

        -- ������������� ����������� ������ ������ ������� ��������� ������ - ��������������� ����� ������� ���������.
        -- ���������� � ������� ������������ �������������� ������ ������
        final_compressor <- newListArray (1,length real_compressor) real_compressor :: IO (IOArray Int String)
        let limit_memory num method = do
              if num > length real_compressor  then return method  else do  -- ���������� ��������� ��� ���������� ����������, ������� ����������� ����
              newMethod <- method.$limit_compression command
              writeArray final_compressor num newMethod
              return newMethod

        -- ������� �������� ����� ����������
        let compressP = de_compress_PROCESS freearcCompress times command limit_memory
        -- ������������������ ��������� ��������, ��������������� ������������������ ���������� `real_compressor`
        let real_crypted_compressor = add_real_encryption real_compressor
            processes = zipWith compressP real_crypted_compressor [1..]
            compressa = case real_crypted_compressor of
                          [_] -> storing_PROCESS |> last processes
                          _   -> storing_PROCESS |> foldl1 (|>) (init processes) |> last processes
        -- ��������� ��������, ���������� ������� �������� �� ����� ������������ ����������� ��� ���������/�������� ������
        let compress_block  =  runFuncP compressa (do x<-receiveP pipe; display x; update_crc x; return x)
                                                  (send_backP pipe)
                                                  (write_to_archive .>>= writeIORef result)
                                                  (val result)
        -- ������� ����� ���������� �������� � ���������� ����������� ������� �����-����� �� �������� ������
        let compress_f  =  if just_copy  then copy_block  else compress_block

        -- ��������� ���� �����-����
        pos_begin <- archiveGetPos archive
        ; times <- uiStartDeCompression "compression"              -- ������� ��������� ��� ����� ������� ��������
        ;   compress_f                                             -- ��������� ������
        ; uiFinishDeCompression times `on` block_type==DATA_BLOCK  -- ������ � UI ������ ����� ��������
        ; uiUpdateProgressIndicator 0                              -- ��������, ��� ����������� ������ ��� ����������
        pos_end   <- archiveGetPos archive

        -- ���������� � ������ ������� ���������� � ������ ��� ��������� �����
        -- ������ �� ������� ������������ � ��� ������
        (Directory dir)  <-  receiveP pipe   -- ������� �� ������� �������� ������ ������ � �����
        crc'             <-  val crc >>== finishCRC     -- �������� ������������� �������� CRC
        origsize'        <-  val origsize
        write_compressor <-  if just_copy then return compressor
                                          else getElems final_compressor >>== add_encryption_info >>== compressionDeleteTempCompressors
        putP backdoor (ArchiveBlock {
                           blArchive     = archive
                         , blType        = block_type
                         , blCompressor  = write_compressor
                         , blPos         = pos_begin
                         , blOrigSize    = origsize'
                         , blCompSize    = pos_end-pos_begin
                         , blCRC         = crc'
                         , blFiles       = error "undefined ArchiveBlock::blFiles"
                         , blIsEncrypted = error "undefined ArchiveBlock::blIsEncrypted"
                       }, dir)


{-# NOINLINE storing_PROCESS #-}
-- |��������������� �������, �������������� ����� Instruction � ����� CompressionData
storing_PROCESS pipe = do
  let send (DataChunk buf len)  =  failOnTerminated  >>  resend_data pipe (DataBuf buf len)  >>  send_backP pipe (buf, i len)
      send  DataEnd             =  resend_data pipe NoMoreData >> return ()
      send _                    =  return ()

  -- �� ��������� ������� ���������� ��������, ��� ������ ������ ���
  ensureCtrlBreak "send DataEnd" (send DataEnd)$ do
    -- ���� ��������������� ����������
    repeat_while (receiveP pipe) (notDataEnd) (send)
  return ()

