{-# OPTIONS_GHC -cpp -XNondecreasingIndentation -XRecursiveDo -XNoMonomorphismRestriction #-}
----------------------------------------------------------------------------------------------------
---- ������� ���������� ������� �������.                                                        ----
---- ���������� �� ArcExtract.hs � ArcCreate.hs (��� ���������� � ������� �������).             ----
----------------------------------------------------------------------------------------------------
module ArcvProcessExtract where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Int
import Data.IORef
import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

#ifdef FREEARC_CELS
import TABI
#endif
import Utils
import Errors
import Process
import FileInfo
import CompressionLib
import Compression
import Encryption
import Options
import UI
import ArhiveStructure
import ArhiveDirectory

{-# NOINLINE decompress_file #-}
-- |���������� ����� �� ������ � �������������� ����������� �������� �������������
-- � ������� ������������� ������ � ������� ������� `writer`
decompress_file decompress_pipe compressed_file writer = do
  -- �� �������� ����������� ��������/������ ����� � ����� ��� ������, ��������� ������� ��������� 0 ������ - �������� �������� ������� ;)
  when (fiSize(cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_while (receiveP decompress_pipe) ((>=0).snd) (uncurry writer)
    failOnTerminated

{-# NOINLINE decompress_PROCESS #-}
-- |�������, ��������������� ����� �� �������
decompress_PROCESS command count_cbytes pipe = do
  cmd <- receiveP pipe
  case cmd of
    Nothing     -> return ()
    Just cfile' -> do
      cfile <- ref cfile'
      state <- ref (error "Decompression state is not initialized!")
      repeat_until $ do
        decompress_block command cfile state count_cbytes pipe
        operationTerminated' <- val operationTerminated
        when operationTerminated' $ do
          sendP pipe (error "Decompression terminated", aFREEARC_ERRCODE_OPERATION_TERMINATED)
        (x,_,_) <- val state
        return (x == aSTOP_DECOMPRESS_THREAD || operationTerminated')


{-# NOINLINE decompress_block #-}
-- |����������� ���� �����-����
decompress_block command cfile state count_cbytes pipe = mdo
  ;   cfile'     <-  val cfile
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
  ;   compressor <-  mapM (limit_decompression command) (blCompressor block) :: IO Compressor
  let startPos  | compressor==aNO_COMPRESSION  =  pos  -- ��� -m0 �������� ������ �������� � ������ ������� � �����
                | otherwise                    =  0
  state =: (startPos, pos, size)
  archiveBlockSeek block startPos
  bytesLeft <- ref (blCompSize block - startPos)

  let reader buf size  =  do aBytesLeft <- val bytesLeft
                             let bytes   = minI (size::Int) aBytesLeft
                             len        <- archiveBlockReadBuf block buf bytes
                             bytesLeft  -= i len
                             count_cbytes  len
                             return len

  let writer (DataBuf buf len)  =  decompress_step cfile state pipe buf len
      writer  NoMoreData        =  return 0

  let limit_memory num method   =  return method    -- ����������� ������ ��� ������ ������������ ������ ��� ������

  -- �������� ���� � ������ ��������� ������������
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (cmd_arcname command) (cfile'.$cfFileInfo.$storedName)

  -- ��������� ������ ������� ������/���������� � �������� ��������� ����������
  let decompress1 = de_compress_PROCESS1 freearcDecompress reader times command limit_memory  -- ������ ������� � ���������
      decompressN = de_compress_PROCESS  freearcDecompress        times command limit_memory  -- ����������� �������� � ���������
      decompressa [p]     = decompress1 p         0
      decompressa [p1,p2] = decompress1 p2        0 |> decompressN p1 0
      decompressa (p1:ps) = decompress1 (last ps) 0 |> foldl1 (|>) (map (\x->decompressN x 0) (reverse$ init ps)) |> decompressN p1 0

  -- � ������� ��������� ����������
  times <- uiStartDeCompression "decompression"  -- ������� ��������� ��� ����� ������� ����������
  ; result <- ref 0   -- ���������� ����, ���������� � ��������� ������ writer
  ; runFuncP (decompressa (map fromJust keyed_compressor)) (fail "decompress_block::runFuncP") (doNothing) (writer .>>= writeIORef result) (val result)
  uiFinishDeCompression times                    -- ������ � UI ������ ����� ��������


{-# NOINLINE de_compress_PROCESS #-}
-- |��������������� ������� �������������� ������ �� ������� �������� ������
-- �� ������� ������ ��������� ��������/����������
--   comprMethod - ������ ������ ������ � �����������, ���� "ppmd:o10:m48m"
--   num - ����� �������� � ������� ��������� ��������
de_compress_PROCESS de_compress times command limit_memory comprMethod num pipe = do
  -- ���������� �� ������� ������, ���������� �� ����������� ��������, �� ��� �� ������������ �� ��������/����������
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", 0)
  let
    -- ��������� "������" ������� ������. �����, ����� ������ ����� � dstlen=0 �� ��������� ���������� ���� �� �������� ���� �� ���� ���� ������ �� ����������� ��������
    read_data prevlen  -- ������� ������ ��� ���������
              dstbuf   -- �����, ���� ����� ��������� ������� ������
              dstlen   -- ������ ������
              = do     -- -> ��������� ������ ���������� ���������� ����������� ���� ��� 0, ���� ������ �����������
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                   -- ���� ��� ���� ������, ���������� �� ����������� ��������
         | srclen>0  ->  copyData buf0 srcbuf srclen  --  �� �������� �� ����������/������������
         | otherwise ->  processNextInstruction       --  ����� �������� �����
        Nothing      ->  return prevlen               -- ���� solid-���� ����������, ������ ������ ���
      where
        -- ����������� ������ �� srcbuf � dstbuf � ���������� ������ ������������� ������
        copyData buf0 srcbuf srclen = do
          let len = srclen `min` dstlen    -- ���������� - ������� ������ �� ����� ���������
          copyBytes dstbuf srcbuf len
          uiReadData num (i len)           -- �������� ��������� ���������
          remains =: Just (buf0, srcbuf+:len, srclen-len)
          case () of
           _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- ���������� ������ ������, ��������� ��� ������ �� ���� ��� �������� ����������/������������
                                 read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- ��������� ��������� ����������
             | len==dstlen -> return (prevlen+len)                                 -- ����� ���������� ��������
             | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)   -- �������� ������� ������ ���������� ��������� ������

        -- �������� ��������� ���������� �� ������ ������� ������ � ���������� �
        processNextInstruction = do
          instr <- receiveP pipe
          case instr of
            DataBuf srcbuf srclen  ->  copyData srcbuf srcbuf srclen
            NoMoreData             ->  do remains =: Nothing;  return prevlen

  -- ��������� ������ ������� ������ �������� ��������/���������� (���������� ���� �������, � ������� �� ����������� read_data)
  let reader  =  read_data 0

  de_compress_PROCESS1 de_compress reader times command limit_memory comprMethod num pipe


{-# NOINLINE de_compress_PROCESS1 #-}
-- |de_compress_PROCESS � ��������������� �������� ������ (����� ������ ������ ��������
-- �� ������ ��� ������� �������� � ������� ����������)
de_compress_PROCESS1 de_compress reader times command limit_memory comprMethod num pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let -- ���������� ����� ������
      showMemoryMap = do printLine$ "\nBefore "++show num++": "++comprMethod++"\n"
                         testMalloc
#ifdef FREEARC_CELS
  let callback p = do
        TABI.dump p
        service <- TABI.required p "request"
        case service of
          -- ��������� ������ ������� ������ �������� ��������/����������
          "read" -> do buf  <- TABI.required p "buf"
                       size <- TABI.required p "size"
                       reader buf size
          -- ��������� ������ �������� ������
          "write" -> do buf  <- TABI.required p "buf"
                        size <- TABI.required p "size"
                        total' += i size
                        uiWriteData num (i size)
                        resend_data pipe (DataBuf buf size)
          -- "�����������" ������ ������������� ������� ������ ����� �������� � ���������� ������
          "quasiwrite" -> do bytes <- TABI.required p "bytes"
                             uiQuasiWriteData num bytes
                             return aFREEARC_OK
          -- ���������� � ������ ������� ���������� ��������/����������
          "time" -> do time <- TABI.required p "time"
                       time' =: time
                       return aFREEARC_OK
          -- ������ (����������������) callbacks
          _ -> return aFREEARC_ERRCODE_NOT_IMPLEMENTED

  let -- ��������� Haskell'������ ���, ���������� �� ��, �� ����� �������� ����������, ������� � ���������� ������/������ ����� ��������
      checked_callback p = do
        operationTerminated' <- val operationTerminated
        if operationTerminated'
          then return CompressionLib.aFREEARC_ERRCODE_OPERATION_TERMINATED   -- foreverM doNothing0
          else callback p
      -- Non-debugging wrapper
      debug f = f
      debug_checked_callback what buf size = TABI.call (\a->fromIntegral `fmap` checked_callback a) [Pair "request" what, Pair "buf" buf, Pair "size" size]
#else
  let -- ��������� ������ ������� ������ �������� ��������/����������
      callback "read" buf size = do res <- reader buf size
                                    return res
      -- ��������� ������ �������� ������
      callback "write" buf size = do total' += i size
                                     uiWriteData num (i size)
                                     resend_data pipe (DataBuf buf size)
      -- "�����������" ������ ������������� ������� ������ ����� �������� � ���������� ������
      -- ��� ����������� ������. �������� ��������� ����� int64* ptr
      callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return aFREEARC_OK
      -- ���������� � ������ ������� ���������� ��������/����������
      callback "time" ptr 0 = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                 time' =: t
                                 return aFREEARC_OK
      -- ������ (����������������) callbacks
      callback _ _ _ = return aFREEARC_ERRCODE_NOT_IMPLEMENTED

  let -- ��������� Haskell'������ ���, ���������� �� ��, �� ����� �������� ����������, ������� � ���������� ������/������ ����� ��������
      checked_callback what buf size = do
        operationTerminated' <- val operationTerminated
        if operationTerminated'
          then return CompressionLib.aFREEARC_ERRCODE_OPERATION_TERMINATED   -- foreverM doNothing0
          else callback what buf size
{-
      -- Debugging wrapper
      debug f what buf size = inside (print (comprMethod,what,size))
                                     (print (comprMethod,what,size,"done"))
                                     (f what buf size)
-}
      -- Non-debugging wrapper
      debug f what buf size = f what buf size
      debug_checked_callback = debug checked_callback
#endif

  -- ���������� �������� ��� ����������
  res <- debug_checked_callback "read" nullPtr (0::Int)  -- ���� ����� ��������� �������� ������ ���������� � ������� ��������� ��������/���������� �� �������, ����� ���������� ��������� ���� �����-������ ������ (� ���� ��� ��������� �������� - �� �������, ����� �� ���������� ���� ����)
  opt_testMalloc command  &&&  showMemoryMap      -- ���������� ����� ������ ��������������� ����� ������� ������
  real_method <- limit_memory num comprMethod     -- ������� ����� ������ ��� �������� ������
  result <- if res<0  then return res
                      else de_compress num real_method (debug checked_callback)
  debug_checked_callback "finished" nullPtr result
  -- ����������
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (real_method,time,total)
  -- ������ � ����������, ���� ��������� ������
  unlessM (val operationTerminated) $ do
    when (result `notElem` [aFREEARC_OK, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED]) $ do
      registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage result, real_method]
      operationTerminated =: True
  -- ������� ����������� ��������, ��� ������ ������ �� �����, � ���������� - ��� ������ ������ ���
  send_backP  pipe aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED
  resend_data pipe NoMoreData
  return ()


-- |��������� ��������� ������ ������������� ������ (writer ��� ������������).
-- ��������� (�������� �� ������ state) ��������:
--   1) block_pos - ������� ������� � ����� ������
--   2) pos       - �������, � ������� ���������� ���� (��� ��� ���������� �����)
--   3) size      - ������ ����� (��� ��� ���������� �����)
-- ��������������, ������� �� ������������ ������ �� ������ buf ������ len, �� ������:
--   1) ���������� � ������ ������ ������, �������������� ���������������� ����� (���� ����)
--   2) �������� �� ����� ������, ����������� � ����� ����� (���� ����)
--   3) �������� ��������� - ������� � ����� ���������� �� ������ ����������� ������,
--        � ������� � ������ ���������� ������ ����� - �� ������ ���������� �� ����� ������
--   4) ���� ���� ���������� ��������� - ���� ��������� �� ���� ����������� �������
--        � �������� ��������� ������� �� ����������
--   5) ���� ��������� ��������������� ���� �������� � ������ ����� ��� � ��� ��������� �����
--        �������� ����� - ���� �������� ���������� ����� ����� � ���, ����� decompress_block
--        ������� � ���������� ����, ��� ����� (�� ������ ��� ������ �� cfile)
--
decompress_step cfile state pipe buf len = do
  (block_pos, pos, size) <- val state
  if block_pos<0   -- ������, ��� ����������� �� ������� ��������, ��� �� ����� ������� � ������� ����� ������
    then return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- ������, ��������, ���� �� �����������. ������������: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- ���������� ������ ���������� ������ � ������ ������
      data_start = buf +: skip_bytes             -- ������ ������, ������������� ���������������� �����
      data_size  = min size (i len-skip_bytes)   -- ���-�� ����, ������������� ���������������� �����
      block_end  = block_pos+i len               -- ������� � �����-�����, ��������������� ����� ����������� ������
  when (data_size>0) $ do    -- ���� � ������ ������� ������, ������������� ���������������� �����
    sendP pipe (data_start, i data_size)  -- �� ������� ��� ������ �� ������ ����� �����������
    receive_backP pipe                    -- �������� ������������� ����, ��� ������ ���� ������������
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- ���� ���� ��� �� ���������� ���������
    then return len     -- �� ���������� ���������� �����
    else do             -- ����� ��������� � ���������� ������� �� ����������
  sendP pipe (error "End of decompressed data", aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED)
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- ��� ��������� ��������, ��� ������ ������� ������ �� ����� ���������� �� ��������� � �� ������ ���� ��������
      state =: (aSTOP_DECOMPRESS_THREAD, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- ���� ����� ���� ��������� � ������ ����� ��� � ����, �� ������
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- ��� �� ������������� ����, ������ � -m0, � � ��� ���� ����������� ���������� ����� ������
        then do state =: (-1, error "undefined state.pos", error "undefined state.size")
                return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- ������� ����, ��� ����� ��������� ���������� ����� �����
        else do state =: (block_pos, pos, size)            -- ����� ���������� ���������� �����,
                decompress_step cfile state pipe buf len   -- ��� � ��������� ���������� ������ �����

-- |������, ��������� ���������� ������ ����� ����������
aSTOP_DECOMPRESS_THREAD = -99


-- |���������, ������������ ��� �������� ������ ���������� �������� ��������/����������
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

{-# NOINLINE resend_data #-}
-- |��������� �������� �������� ������ ����������/������������ ��������� ��������� � �������
resend_data pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- ���������� ���������� ����������� ����, ������������ �� ��������-�����������
resend_data pipe x@NoMoreData  =  sendP pipe x  >>  return 0

