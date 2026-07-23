{-# OPTIONS_GHC -XNondecreasingIndentation #-}
----------------------------------------------------------------------------------------------------
---- ������ �� ���������� ��������� �����.                                                    ------
---- ����� ������������ �� ���� ������������������ ������, ������ �� ������� ����� ����:      ------
----   * ������ ������                                                                        ------
----   * ������ �������� ������                                                               ------
----   * ������ ��������� ������ (recovery record, ����� ���������� �� ������ � �.�.)         ------
---- ����� ������� ���������� ����� (�� ���� ������, ����� ������ ������), ������������       ------
----   ����������, ����������� ��������� � ��������� ���� ���� ���� � ������ �����            ------
----   � ���������� ������.                                                                   ------
---- ���� ������ �������� ��������� ���:                                                      ------
----   * ������ � ������ ��������� ������ ������                                              ------
----   * ������, ������ � ������ ������������ ��������� ������                                ------
----------------------------------------------------------------------------------------------------
module ArhiveStructure where

import Prelude hiding (catch)
import Control.Monad
import Data.Word
import Data.Maybe
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Storable

import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import Compression
import Encryption
import Options

-- |��������� ��� ������ ������������ ��������� ������
aSIGNATURE = make4byte 65 114 67 1 :: Word32

-- |���������, ������������ � ����� ������ ������ - �� ��� ����� �������� �������� ����, + ������ ����������
aARCHIVE_SIGNATURE = (aSIGNATURE, aARCHIVE_VERSION)

-- |������� ���� � ����� ������ ����������� � ������ ���������, � ������� ���������� ���������� ���������� ����� ������
aSCAN_MAX :: Num a => a
aSCAN_MAX = 4096

-- ���� ������������ �����
aTAG_END = 0::Integer   -- ^��� ��������� ������������ �����


----------------------------------------------------------------------------------------------------
---- ������/������/����� ����������� ���������� ����� ������ ---------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� � ����� ���������� ���������� ����� `block` � CRC ����� �����������
archiveWriteBlockDescriptor block (receiveBuf,sendBuf) = do
  crc  <- ref aINIT_CRC
  let sendBuf_UpdatingCRC buf size len  =  do crc .<- updateCRC buf len;  sendBuf buf size len
  ByteStream.writeAll receiveBuf sendBuf_UpdatingCRC (return ())
    -- ��������� ����� �������� � ������ ������� ������������ ����� - ������� ���� ����������� ������, ����� ��� ����������:
    --   ���������, ��� �����, �������������� ����������, ������ � ������������� � ����������� ����, CRC ������������� ������
    (aSIGNATURE, blType block, blCompressor block, blOrigSize block, blCompSize block, blCRC block)
  acrc <- val crc
  -- ����� ����������� ������������ CRC ������ �����������
  ByteStream.writeAll receiveBuf sendBuf (return ())
    (finishCRC acrc)

-- |��������� �� ������ ���������� ���������� ����� � ������������ ��� � ������ ����,
-- ��� � ������ ���� ���������� ��������� �� �������� `arcpos`
archiveReadBlockDescriptor archive arcpos buf bufsize = do
  -- �������� ������� CRC ������ �����������
  right_crc  <- peekByteOff buf (bufsize - sizeOf (undefined::CRC))
  descriptor_crc <- calcCRC buf (bufsize - sizeOf (undefined::CRC))
  if (descriptor_crc/=right_crc)
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0354 block descriptor at pos %1 is corrupted", show arcpos]
    else do
  -- ���������� ���������� �����������:
  -- ���������, ��� �����, �������������� ����������, ������ � ������������� � ����������� ����, CRC ������������� ������
  (sign, block_type, compressor, origsize, compsize, crc)  <-  ByteStream.readMemory buf bufsize
  let pos   =  blDecodePosRelativeTo arcpos compsize  -- ������� � ������ ������ �����
      block =  ArchiveBlock archive block_type compressor pos origsize compsize crc undefined (enc compressor)
  if (sign/=aSIGNATURE || pos<0)
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0355 %1 is corrupted", block_name block]
    else return$ Right block

{-# NOINLINE archiveWriteBlockDescriptor #-}
{-# NOINLINE archiveReadBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- ������������ ������������ ������ � ������� ��������� ������ -----------------------------------
----------------------------------------------------------------------------------------------------

-- |������������ ������ � ������� ��������� ������.
-- ������ �� ������ � ���������� ������-FOOTER BLOCK, ���������� ��� ��������� �����.
-- ��������� ������ ����� � ����� ������� �� 8 ��, ���� ����������� ������ � ���� ������
-- � ����� ���������, ��� ���������� � ��� ���� � �������.
findBlocksInBrokenArchive arcname = do
  archive <- archiveOpen arcname
  arcsize <- archiveGetSize archive
  -- �������� ����� � 8 �� + 2 ���� �� 4 ����� ��� ��������� ���������� ������ �����������
  allocaBytes (aHUGE_BUFFER_SIZE+2*aSCAN_MAX) $ \buf -> do
  blocks <- withList $ scanArchiveSearchingDescriptors archive buf arcsize
  if null blocks
    then registerError$ BROKEN_ARCHIVE arcname ["0356 archive directory not found"]
    else do
  -- ���������� ���������� FOOTER_BLOCK, ���� �� ������� � ������
  --if blType (head blocks) == FOOTER_BLOCK
  --  then return (archive, (head blocks) {ftBlocks=reverse blocks})
  --  else do
  let pseudo_footer = FooterBlock
        { ftBlocks   = reverse blocks
        , ftLocked   = False
        , ftComment  = ""
        , ftRecovery = ""
        , ftSFXSize  = minimum (map blPos blocks)   -- ������ SFX = ������� HEADER_BLOCK � ������
        }
  return (archive, pseudo_footer)

-- ����� � ������ ����������� �����, ��� ����������� ����������
-- �� ������� <pos, � ������� ��� ����������� � ������ found
scanArchiveSearchingDescriptors archive buf pos found =
  when (pos>0) $ do
    -- ��� ��������� ������ ������ ��������� ��������� ����� ���� �� 8-�������� �������
    -- � ���� ���������� ������� � ���� ������� base_pos �� ������� pos-1
    let base_pos = (pos-1) `roundDown` aHUGE_BUFFER_SIZE
    archiveSeek archive base_pos
    -- ������ �� 4 ������ ������ ������������ ����� ����� ����������� ���������
    -- ����������, ������������ � ����� ����� ������ (����� ����������� �������� ������ 4 �����)
    len <- archiveReadBuf archive buf (i$ pos-base_pos+aSCAN_MAX)
    memset (buf+:len) 0 aSCAN_MAX  -- �� ������ ������ �������� ����������� 4096-� ������
    -- scanMem ���������� ������� � ������, ����� ������� ��� ����������� ��� ����������
    newpos <- scanMem archive base_pos found buf ((i$ pos-base_pos) `min` len)
    -- �������� ������� ���������� ��� ������ ������������ � ���������� �����
    scanArchiveSearchingDescriptors archive buf newpos found

-- ����������� ����� buf � ������� ������������, ������������ � ������ len ������ ����� ������
scanMem archive base_pos found buf len = do
  pos' <- ref base_pos
  whenRightM_ (archiveFindBlockDescriptor archive base_pos buf (len+aSCAN_MAX) len) $ \block -> do
    -- ������ ���������� ����� block. ������, ���������� ����� ����� ���� ���������
    -- ��� ��������, � ���� ��� ���� �������� - �� �������� ������� ����� ������ � ���
    pos' =: blPos block
    -- ��������, �� ����� ����� ������, ��������� ����� ����� �������� "�������������� �������"
    -- � ��������������, �� ����� ������ ����� �� ����� ����� ������ ����� ��������� ������
    -- ������ �����. � ������ �� �������, ��� ������ ����� ��������� ������ ������ ����� � ��������
    -- ����, � ����� �� ������� ��� ����� � ������ ������ ������ ������ :)
    -- ��� ������� - "���������� ������" ����� �������������� ������
    -- when (blType block == DIR_BLOCK) $ do
    --   data_blocks <- archiveReadDir_OnlyBlocks
    --   pos' =: minpos data_blocks
    found <<= block
  -- ���� ����� �������� ��� �� ���������������� ������, �� ���������� ����� � ���,
  -- ����� - ������� � ����������� ����� ������
  pos <- val pos'
  if pos > base_pos
    then scanMem archive base_pos found buf (i$ pos-base_pos)
    else return pos

-- |���������� ����� (���������) ���������� ����� � ���������� ������.
-- ����� ����� ������ � ����� - size, �� ��� ���� ��� ���������� ������ �����������,
-- ������������ � ������ len ������ �����.
archiveFindBlockDescriptor archive base_pos buf size len =
  go ((size-sizeOf(aSIGNATURE)) `max` (len-1)) defaultError
    where
  go pos err | pos<0     = return$ Left err
             | otherwise = do
       x <- peekByteOff buf pos
       if x==aSIGNATURE
         then do -- ������� ��������� ����������� �� ������ pos, ��������� �������� �� ��� ����������
                 res <- archiveReadBlockDescriptor
                          archive            -- ���� ������
                          (base_pos+i pos)   -- ������� � �������� ����� ������������� �����������
                          (buf+:pos)         -- ����� � ������ ������������� �����������
                          (size-pos)         -- ����������� ��������� ������ ����� �����������
                 case res of
                   Left  err -> go (pos-1) err
                   Right _   -> return res
         else go (pos-1) err
  -- ��������� �� ������, ����������� ���� � ����� ������ �� ������� �� ������ ����������
  defaultError = BROKEN_ARCHIVE (archiveName archive) ["0357 archive signature not found at the end of archive"]

{-# NOINLINE findBlocksInBrokenArchive #-}
{-# NOINLINE scanArchiveSearchingDescriptors #-}
{-# NOINLINE scanMem #-}
{-# NOINLINE archiveFindBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- ������ ���������� ����� ������ (HEADER_BLOCK) -------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� � ��������� ���� ������ (HEADER_BLOCK) ��������� ������
archiveWriteHeaderBlock (receiveBuf,sendBuf) = do
  ByteStream.writeAll receiveBuf sendBuf (return ()) $
    aARCHIVE_SIGNATURE


----------------------------------------------------------------------------------------------------
---- ������ � ������ RECOVERY ����� ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� RECOVERY ����
archiveWriteRecoveryBlock :: (ByteStream.BufferData a) =>  Maybe a -> Ptr CChar -> Int -> (ByteStream.RecvBuf, ByteStream.SendBuf) -> IO ()
archiveWriteRecoveryBlock moreinfo buf size (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  case moreinfo of
    Just info -> ByteStream.write stream info
    Nothing   -> return ()
  ByteStream.writeBuf stream buf size
  ByteStream.closeOut stream


----------------------------------------------------------------------------------------------------
---- ������ � ������ ���������� ����� ������ (FOOTER_BLOCK) ------------------------------------------
----------------------------------------------------------------------------------------------------

-- |����������, ������������ � FOOTER BLOCK
data FooterBlock = FooterBlock
       { ftBlocks     :: ![ArchiveBlock]     -- ������ ������ � ������ (�� ����������� ������ ������)
       , ftLocked     :: !Bool               -- ����� ������ �� ���������?
       , ftComment    :: !String             -- ����������� ������
       , ftRecovery   :: !String             -- ��������� recovery info
       , ftSFXSize    :: !FileSize           -- ������ SFX-������, ��������������� ���������� ������ (����������� ��� ����� ������, �������������� ������� ����� ������)
       }

-- |�������� FOOTER_BLOCK
archiveWriteFooterBlock control_blocks arcLocked arcComment (arcRecovery::String) arcpos (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let utf8comment  =  ByteStream.toUTF8List arcComment
  ByteStream.write        stream (map (blockToTuple arcpos) control_blocks)   -- ������� �������� ����������� ������,
  ByteStream.write        stream arcLocked                                    -- ... ������� �������� ������ �� ���������
  ByteStream.writeInteger stream 0                                            -- ... ����������� ������ � ������ ������� - �����������
  ByteStream.write        stream arcRecovery                                  -- ... ����� recovery ���������
  ByteStream.writeInteger stream (length utf8comment)                         -- ... ����������� ������ (�������� ��� ������, ��������� ��� ���� ���� ���������� ����� � ����������� ����� ������� ����� ��������� ������� �������)
  ByteStream.writeList    stream utf8comment                                  --     -.-
  ByteStream.closeOut     stream

-- |��������� ���������� �� FOOTER_BLOCK
archiveReadFooterBlock footer@ArchiveBlock {
                                   blArchive  = archive
                                 , blType     = block_type
                                 , blPos      = pos
                                 , blOrigSize = origsize
                               }
                       decryption_info = do
  when (block_type/=FOOTER_BLOCK) $
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0358 last block of archive is not footer block"]
  withPool $ \pool -> do   -- ���������� ��� ������, ����� ������������� ���������� ���������� ������ ��� ������
    (buf,size) <- archiveBlockReadAll pool decryption_info footer  -- �������� � ����� ������������� ������ �����
    stream <- ByteStream.openMemory buf size
    control_blocks <- ByteStream.read stream      -- ��������� �������� ����������� ������,
    locked         <- ByteStream.read stream      -- ... ������� �������� ������ �� ���������
    oldComment     <- ByteStream.readInteger stream >>= ByteStream.readList stream >>== map (toEnum.i :: Word32 -> Char)  -- ... � ����������� ������ (������ ��� ������, ��������� ��� ���� ���� ���������� ����� � ����������� ����� ������� ����� ��������� ������� �������)
    isEOF          <- ByteStream.isEOFMemory stream  -- ������ ������ ��������� �� ���������� ���������� � recovery record
    recovery       <- not isEOF &&& ByteStream.read stream  -- ... ��������� RECOVERY ����������, ����������� � ������
    isEOF          <- ByteStream.isEOFMemory stream
    comment        <- not isEOF &&& (ByteStream.readInteger stream >>= ByteStream.readList stream >>== ByteStream.fromUTF8)  -- ... � ����������� ������ (������ ��� ������, ��������� ��� ���� ���� ���������� ����� � ����������� ����� ������� ����� ��������� ������� �������)
    ByteStream.closeIn stream
    let blocks = map (tupleToBlock archive pos) control_blocks   -- ������������� ��������� ArchiveBlock �� ���������� ������
    return FooterBlock
             { ftBlocks   = blocks++[footer]
             , ftLocked   = locked
             , ftComment  = comment ||| oldComment
             , ftRecovery = recovery
             , ftSFXSize  = minimum (map blPos blocks)   -- ������ SFX = ������� HEADER_BLOCK � ������
             }

{-# NOINLINE archiveWriteFooterBlock #-}
{-# NOINLINE archiveReadFooterBlock #-}


----------------------------------------------------------------------------------------------------
---- ���� ������ (���� ������, ������� ��� ���������) ----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���� ������
data ArchiveBlock = ArchiveBlock
       { blArchive     :: !Archive      -- �����, � �������� ����������� ������ ����
       , blType        :: !BlockType    -- ��� �����
       , blCompressor  :: !Compressor   -- ����� ������
       , blPos         :: !FileSize     -- ������� ����� � ����� ������
       , blOrigSize    :: !FileSize     -- ������ ����� � ������������� ����
       , blCompSize    :: !FileSize     -- ������ ����� � ����������� ����
       , blCRC         :: !CRC          -- CRC ������������� ������ (������ ��� ��������� ������)
       , blFiles       ::  Int          -- ���������� ������ (������ ��� ������ ������)
       , blIsEncrypted ::  Bool         -- ���� ����������?
       }

instance Eq ArchiveBlock where
  (==)  =  map2eq$ map5 (blPos, blOrigSize, blCompSize, archiveName.blArchive, blCompressor)    -- not exact! block with only directories and empty files may have size 0!!!

-- |��������������� ������� ��� ���������� ���� blIsEncrypted �� blCompressor
enc = any isEncryption

-- |��� �������� � ������ ���������� �� �������� ������. ������� ����� ������������ � �����
-- ������������ `arcpos` - ������� � ������ ���� �����, � ������� ����������� ��� ����������
blockToTuple              arcpos (ArchiveBlock _ t c p o s crc f e) = (t,c,arcpos-p,o,s,crc)
tupleToBlock     archive arcpos (t,c,p,o,s,crc) = (ArchiveBlock archive t c (arcpos-p) o s crc undefined (enc c))
tupleToDataBlock archive arcpos   (c,p,o,s,f)   = (ArchiveBlock archive DATA_BLOCK c (arcpos-p) o s 0 f (enc c))

-- ��������� ������� ��� (��)����������� ������� ����� ������������ ������� ����� � ������
blEncodePosRelativeTo arcpos arcblock  =  arcpos - blPos arcblock
blDecodePosRelativeTo arcpos offset    =  arcpos - offset

-- |�������� �����
block_name block  =  (case (blType block) of
                          DESCR_BLOCK    -> "block descriptor"
                          HEADER_BLOCK   -> "header block"
                          DATA_BLOCK     -> "data block"
                          DIR_BLOCK      -> "directory block"
                          FOOTER_BLOCK   -> "footer block"
                          RECOVERY_BLOCK -> "recovery block"
                          _              -> "block of unknown type"
                      ) ++ " at pos "++ show (blPos block)

-- |��� ����� ������ (�������� ��������� ������ � �����, ��������� ��� ������������ � �����!)
data BlockType = DESCR_BLOCK       -- ^��� ����������� ����� ������  (������������ ����� ������� ���������� �����, �� ���� ����, ����� DATABLOCK)
               | HEADER_BLOCK      -- ^��� ���������� ����� ������   (������������� ��� ��������� ��������� �����)
               | DATA_BLOCK        -- ^��� ����� ������
               | DIR_BLOCK         -- ^��� ����� ��������
               | FOOTER_BLOCK      -- ^��� ��������� ����� ������    (����������� ������ ������ � ������)
               | RECOVERY_BLOCK    -- ^��� ����� � recovery info
               | UNKNOWN_BLOCK     -- ������� �������������� �������� ��� ���� ���������������� ���� ������� ��������� ����� �����
               | UNKNOWN_BLOCK2
               | UNKNOWN_BLOCK3
     deriving (Eq,Enum)

instance ByteStream.BufferData BlockType  where
  write buf = ByteStream.writeInteger buf . fromEnum
  read  buf = ByteStream.readInteger  buf >>== toEnum

-- �������� � ������� ������
archiveBlockSeek    block pos       =  archiveSeek    (blArchive block) (blPos block + pos)
archiveBlockRead    block size      =  archiveRead    (blArchive block) size
archiveBlockReadBuf block buf size  =  archiveReadBuf (blArchive block) buf size

-- |�������� �����, ��������� � ���� ���������� ����� � ��������� CRC
archiveBlockReadAll pool
                    decryption_info
                    block@ArchiveBlock {
                              blArchive     = archive
                            , blType        = block_type
                            , blCompressor  = compressor
                            , blPos         = pos
                            , blCRC         = right_crc
                          } = do
  let origsize = i$ blOrigSize block
      compsize = i$ blCompSize block
  (origbuf, decompressed_size)  <-  decompressInMemory pool compressor decryption_info archive pos compsize origsize
  crc <- calcCRC origbuf origsize
  when (crc/=right_crc || decompressed_size/=origsize) $ do
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0359 %1 failed decompression", block_name block]
  return (origbuf, origsize)

-- |�������� ����� � ��������� � ���� ���������� �����. �� ��������� CRC � �� ������������� ������!
archiveBlockReadUnchecked pool block = do
  when (blCompressor block/=aNO_COMPRESSION) $ do
    registerError$ BROKEN_ARCHIVE (archiveName$ blArchive block) ["0360 %1 should be uncompressed", block_name block]
  archiveMallocReadBuf pool (blArchive block) (blPos block) (i$ blOrigSize block)

-- |�������� ����� � ��������� � ���� ������ �� ������
archiveMallocReadBuf pool archive pos size = do
  buf         <- pooledMallocBytes pool (size+8)  -- +8 - ��-�� ����������� � ByteStream :(
  archiveSeek    archive pos
  archiveReadBuf archive buf size
  return buf

-- |������������ � ������ ����, ������� ����� ���� �������� ����������� ����������� � �������� ����������
decompressInMemory mainPool compressor decryption_info archive pos compsize origsize = do
  withPool $ \tempPool -> do
  let process srcbuf srcsize [] = return (srcbuf, srcsize)
      process srcbuf srcsize (algorithm:algorithms) = do
        let (dstsize, pool) = if null algorithms
                                then (origsize, mainPool)
                                else ((max compsize origsize)*2+100*kb, tempPool)
        dstbuf <- pooledMallocBytes pool (dstsize+8)  -- +8 - ��-�� ����������� � ByteStream :(
        decompressed_size <- decompressMem algorithm srcbuf srcsize dstbuf dstsize
        pooledReallocBytes tempPool srcbuf 0
        process dstbuf decompressed_size algorithms
  --
  if compressor==aNO_COMPRESSION
    then do compbuf <- archiveMallocReadBuf mainPool archive pos (compsize+8)
            return (compbuf, compsize)
    else do
  -- ��������� ��������� ���������� �������, ������������ ��� �����������
  keyed_compressor <- generateDecryption compressor decryption_info
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (archiveName archive) ""
  -- ��������� �������� ���� �� ������
  compbuf <- archiveMallocReadBuf tempPool archive pos compsize
  process compbuf compsize (reverse (map fromJust keyed_compressor))


{-# NOINLINE archiveBlockReadAll #-}
{-# NOINLINE archiveMallocReadBuf #-}

