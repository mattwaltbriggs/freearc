{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- ��������, ���������� � ���������� CRC.                                                     ----
---- ���� ������ CompressionMethod, Compressor, UserCompressor - �������� ������ ������.        ----
---- ��������� � ����������� �� �� �����������, ������������ ��� �������� ������.               ----
----------------------------------------------------------------------------------------------------
module Compression (module Compression, CompressionLib.decompressMem) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Ptr
import System.IO.Unsafe

#ifdef FREEARC_CELS
import qualified TABI
#endif
import qualified CompressionLib
import Utils
import Errors
import Files
import qualified ByteStream


-- |����� ������ ��� ������������ � ��� ���������
type CompressionMethod  =  CompressionLib.Method

-- ������ "������", �������������� ��������, � �� ����� CompressionLib
aSTORING              = "storing"
aFAKE_COMPRESSION     = "fake"
aCRC_ONLY_COMPRESSION = "crc"

-- |�������� (�����������������) ������ ������.
isFakeMethod             =  any_function [(==aFAKE_COMPRESSION), (==aCRC_ONLY_COMPRESSION)] . method_name
-- |LZP ����� ������.
isLZP_Method     method  =  method_name method == "lzp"
-- |Tornado ����� ������.
isTornado_Method method  =  method_name method == "tor"
-- |DICT ����� ������.
isDICT_Method    method  =  method_name method == "dict"
-- |TTA ����� ������.
isTTA_Method     method  =  method_name method == "tta"
-- |MM ����� ������.
isMM_Method      method  =  method_name method == "mm"
-- |JPG ����� ������.
isJPG_Method     method  =  method_name method == "jpg"
-- |GRZip ����� ������.
isGRZIP_Method   method  =  method_name method == "grzip"
-- |�����, �������� ����� �������� ����� �� ������ �� �����-���� (bmf, tta � ��� �����)
isNonSolidMethod         =  CompressionLib.compressionIs "nosolid?"
-- |����� ������� ����� �������� (>10 mb/s �� 1��� ����������)
isVeryFastMethod         =  CompressionLib.compressionIs "VeryFast?"
-- |������� ����� ����������
isFastDecMethod          =  not . any_function [(=="ppmd"), (=="ppmm"), (=="pmm"), isEXTERNAL_Method] . method_name
-- |����� ������, ����������� ������� ����������
isEXTERNAL_Method        =  CompressionLib.compressionIs "external?"
-- |����� ����������.
isEncryption             =  CompressionLib.compressionIs "encryption?"


-- |������������������ ���������� ������, ������������ ��� ��������� ������
type Compressor = [CompressionMethod]

-- |����� "storing" (-m0)
aNO_COMPRESSION = [aSTORING] :: Compressor

-- |����� ������� ������ ��� ��� ������ ������
aCOMPRESSED_METHOD = split_compressor "tor:8m:c3"

-- |��� - �������� ����������, ���� � ��� ����� ���� ����� ������ � �� - ��������
isFakeCompressor (method:xs)  =  isFakeMethod method  &&  null xs

-- |��� - fake ����������, ���� � ��� ����� ���� ����� ������ � �� - "fake"
isReallyFakeCompressor (method:xs)  =  method_name method == aFAKE_COMPRESSION  &&  null xs

-- |��� - LZP ����������, ���� � ��� ����� ���� ����� ������ � �� - LZP
isLZP_Compressor (method:xs)  =  isLZP_Method method  &&  null xs

-- |��� - ����� ������� ���������, ���� � ��� ����� ����, ����� ������� ����� ������.
isVeryFastCompressor (method:xs)  =  isVeryFastMethod method  &&  null xs

-- |��� - ������� �����������, ���� �� �������� ������ ������� ������ ����������
isFastDecompressor :: [String] -> Bool
isFastDecompressor = all isFastDecMethod


-- |����� ����������� � ����������� �� ���� �������������� ������.
-- ������ ������� ������ ��������� � ��������� ����������, ������������
-- �� ��������� (��� ������ ���� ������ �����, �� ��������� � ������ ����)
type UserCompressor = [(String,Compressor)]  -- ������ ���������� ���� "$text->m3t, $exe->m3x, $compressed->m0"

getCompressors :: UserCompressor -> [Compressor]
getCompressors = map snd

getMainCompressor :: UserCompressor -> Compressor
getMainCompressor = snd.head

-- |��� - ����� Storing, ���� � ��� ������ ���� ���������� aNO_COMPRESSION ��� ������ ���� �����
isStoring ((_,compressor):xs)  =  compressor==aNO_COMPRESSION  &&  null xs

-- |��� - fake compression, ���� � ��� ������ ���� �������� ���������� ��� ������ ���� �����
isFakeCompression ((_,compressor):xs)  =  isFakeCompressor compressor  &&  null xs

-- |��� - LZP compression, ���� � ��� ������ ���� LZP ���������� ��� ������ ���� �����
isLZP_Compression ((_,compressor):xs)  =  isLZP_Compressor compressor  &&  null xs

-- |��� ����� ������� ��������, ���� � ��� ������������ ������ ����� ������� ���������� ��� ������ ���� �����
isVeryFastCompression :: [(a, [CompressionLib.Method])] -> Bool
isVeryFastCompression = all (isVeryFastCompressor.snd)

-- |��� ������� ����������, ���� � ��� ������������ ������ ������� ������������ ��� ������ ���� �����
isFastDecompression :: [(a, [String])] -> Bool
isFastDecompression = all (isFastDecompressor.snd)

-- |����� ����������, �������� ���������� ��� ������ ���� `ftype`.
-- ���� ���������� ��� ������ ����� ���� �� ������ � ������ - ���������� ����������
-- �� ���������, ���������� � ������ ������� ������
findCompressor ftype list  =  lookup ftype list  `defaultVal`  snd (head list)

-- |��� ������ � ���������� ������ ���������� �� �������������� ���������� ������.
instance ByteStream.BufferData Compressor where
  write buf x  =  ByteStream.write buf (join_compressor x)
  read  buf    =  ByteStream.read  buf  >>==  split_compressor


----------------------------------------------------------------------------------------------------
----- �������� ��� ����������� ������                                                          -----
----------------------------------------------------------------------------------------------------

class Compression a where
  getCompressionMem              :: a -> Integer
  getDecompressionMem            :: a -> Integer
  getBlockSize                   :: a -> MemSize
  getDictionary                  :: a -> MemSize
  setDictionary                  :: MemSize -> a -> a
  limitCompressionMem            :: MemSize -> a -> a
  limitDecompressionMem          :: MemSize -> a -> a
  limitDictionary                :: MemSize -> a -> a
  limitCompressionMemoryUsage    :: MemSize -> a -> a
  limitDecompressionMemoryUsage  :: MemSize -> a -> a

-- |���������� ������� �� CompressionLib, ���������� Method, � �������, ���������� CompressionMethod
liftSetter action  method | aSTORING ==  method   =  method
liftSetter action  method | isFakeMethod method   =  method
liftSetter action  method                         =  action method

-- |���������� ������� �� CompressionLib, ������������ Method, � �������, ������������ CompressionMethod
liftGetter action  method | aSTORING ==  method   =  0
liftGetter action  method | isFakeMethod method   =  0
liftGetter action  method                         =  action method

instance Compression CompressionMethod where
  getCompressionMem              =i.liftGetter   CompressionLib.getCompressionMem
  getDecompressionMem            =i.liftGetter   CompressionLib.getDecompressionMem
  getBlockSize                   =  liftGetter   CompressionLib.getBlockSize
  getDictionary                  =  liftGetter   CompressionLib.getDictionary
  setDictionary                  =  liftSetter . CompressionLib.setDictionary
  limitCompressionMem            =  liftSetter . CompressionLib.limitCompressionMem
  limitDecompressionMem          =  liftSetter . CompressionLib.limitDecompressionMem
  limitDictionary                =  liftSetter . CompressionLib.limitDictionary
  limitCompressionMemoryUsage    =  limitCompressionMem
  limitDecompressionMemoryUsage  =  const id

instance Compression Compressor where
  getCompressionMem              =  calcMem getCompressionMem
  getDecompressionMem            =  calcMem getDecompressionMem
  getBlockSize                   =  maximum . map getBlockSize
  getDictionary                  =  maximum . map getDictionary
  setDictionary                  =  mapLast . setDictionary
  limitCompressionMem            =  map . limitCompressionMem
  limitDecompressionMem          =  map . limitDecompressionMem
  limitDictionary                =  compressionLimitDictionary
  limitCompressionMemoryUsage    =  compressionLimitMemoryUsage
  limitDecompressionMemoryUsage  =  genericLimitMemoryUsage getDecompressionMem

instance Compression UserCompressor where
  -- ���������� ������������ ����������� ������ / ������ ����� � �������� UserCompressor
  getCompressionMem              =  maximum . map (getCompressionMem   . snd)
  getDecompressionMem            =  maximum . map (getDecompressionMem . snd)
  getBlockSize                   =  maximum . map (getBlockSize        . snd)
  getDictionary                  =  maximum . map (getDictionary       . snd)
  -- ���������� ������� / ���������� ������������ ��� ������/���������� ������
  -- ����� ��� ���� �������, �������� � UserCompressor
  setDictionary                  =  mapSnds . setDictionary
  limitCompressionMem            =  mapSnds . limitCompressionMem
  limitDecompressionMem          =  mapSnds . limitDecompressionMem
  limitDictionary                =  mapSnds . limitDictionary
  limitCompressionMemoryUsage    =  mapSnds . limitCompressionMemoryUsage
  limitDecompressionMemoryUsage  =  mapSnds . limitDecompressionMemoryUsage


-- |����������� ����� ������, ����������� ��� ��������/����������
compressorGetShrinkedCompressionMem    = maximum . map (compressionGetShrinkedCompressionMem . snd)
compressorGetShrinkedDecompressionMem  = maximum . map (compressionGetShrinkedDecompressionMem . snd)
compressionGetShrinkedCompressionMem    = maximum . map (getCompressionMem :: CompressionMethod -> Integer)
compressionGetShrinkedDecompressionMem  = maximum . map (getDecompressionMem :: CompressionMethod -> Integer)

-- |���������� ������� ��� ������� ����������, ��������� ��� ������ ����� ������� ���������,
-- ������� ����� ����������� ������� ������ (���� precomp). ����� ���������� ����������
-- ����� ���, �� �� ������ ��� ����������� ��� ������� :)
compressionLimitDictionary mem (x:xs) =  new_x : (not(isEXTERNAL_Method new_x)  &&&  compressionLimitDictionary mem) xs
                                             where new_x = limitDictionary mem x
compressionLimitDictionary mem []     =  []

-- |��������� ����������� � ������ ������� ��������� � ������� �� mem
-- � ����� ��������� ����� ���� ������ tempfile, ���� ����������
compressionLimitMemoryUsage mem  =  genericLimitMemoryUsage getCompressionMem mem . map (limitCompressionMem mem)

-- |��������� ������ tempfile ����� ����������� ������, �������� �� �� "��������",
-- ����������� � memory_limit+5% (��� ���� "���������" ��������� �� ������ �������� ����� ���������).
-- ��� ���� ��� dict/dict+lzp ������������ ������ ���� ������ (blocksize*2 �� ���, blocksize/2 �� ������),
-- � external compressors �������� ����������� ������
genericLimitMemoryUsage getMem memory_limit = go (0::Double) ""
  where go _   _    []      =  []
        go mem prev (x:xs) | isEXTERNAL_Method x          =  x: go 0            x xs
                           | mem+newMem < memlimit*1.05   =  x: go (mem+newMem) x xs
                           | otherwise                    =  "tempfile":x: go newMem x xs

           where newMem | mem==0 && isDICT_Method x             =  realToFrac (getBlockSize x) / 2
                        | isDICT_Method prev && isLZP_Method x  =  0
                        | otherwise                             =  realToFrac$ getMem x
                 memlimit = realToFrac memory_limit

-- |��������� ����������� � ������ ������� ���������� ������ � ������ �� ���������.
-- �� �������� �� compressionIs "external?" � ������ ������ dict/dict+lzp
calcMem getMem  = maximum . map getMemSum . splitOn isEXTERNAL_Method
  where getMemSum (x:y:xs) | isDICT_Method x && isLZP_Method y  =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)   | isDICT_Method x                    =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)                                        =  i(getMem x) + getMemSum xs
        getMemSum []                                            =  0::Integer

-- |������� ��� ���������� � "tempfile" �� ������ ��������� ������.
compressionDeleteTempCompressors = filter (/="tempfile")


----------------------------------------------------------------------------------------------------
----- (De)compression of data stream                                                           -----
----------------------------------------------------------------------------------------------------

-- |��������� �������� ��� ��������� ���������� ������.
freearcCompress   :: Int -> String -> (String -> Ptr CChar -> Int -> IO Int) -> IO Int
freearcCompress   num method | isFakeMethod method =  eat_data
freearcCompress   num method                       =  CompressionLib.compress method

-- |��������� ���������� ��� ��������� ���������� ������.
freearcDecompress num method | isFakeMethod method =  impossible_to_decompress   -- ��� ���� ������ ������ �� �������� ����������
freearcDecompress num method                       =  CompressionLib.decompress method

-- |������ ��, �� ����� ������, � CRC ��������� � ������ ����� ;)
eat_data :: (String -> Ptr CChar -> Int -> IO Int) -> IO Int
eat_data callback = do
  allocaBytes (fromIntegral aBUFFER_SIZE) $ \buf -> do  -- ���������� `alloca`, ����� ������������� ���������� ���������� ����� ��� ������
    let go = do
#ifdef FREEARC_CELS
          len <- TABI.call (\a->fromIntegral `fmap` callback a) [TABI.Pair "request" "read", TABI.Pair "buf" buf, TABI.Pair "size" (aBUFFER_SIZE::MemSize)]
#else
          len <- callback "read" buf (fromIntegral aBUFFER_SIZE)
#endif
          if (len>0)
            then go
            else return len   -- ��������� 0, ���� ������ ���������, � ������������� �����, ���� ��������� ������/������ ������ �� �����
    go  -- ���������� ���������

impossible_to_decompress callback = do
  return CompressionLib.aFREEARC_ERRCODE_GENERAL   -- ����� ���������� ������, ��������� ���� �������� (FAKE/CRC_ONLY) �� �������� ����������


----------------------------------------------------------------------------------------------------
----- CRC calculation ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |CRC �����
type CRC  = CUInt
aINIT_CRC = 0xffffffff  :: CRC
updateCRC addr len  =  c_UpdateCRC addr (i len)
finishCRC = xor aINIT_CRC

-- |��������� CRC ������ � ������
calcCRC addr len  =  updateCRC addr len aINIT_CRC  >>==  finishCRC

-- |��������� CRC ��-unicode ������ (������� � ������ 0..255)
crc32 str  =  unsafePerformIO$ withCStringLen str (uncurry calcCRC)

-- |Fast C routine for CRC-32 calculation
foreign import ccall safe "Environment.h UpdateCRC"
   c_UpdateCRC :: Ptr CChar -> CUInt -> CRC -> IO CRC


-------------------------------------------------------------------------------------------------------------
-- Encode/decode compression method for parsing options/printing info about selected compression method -----
-------------------------------------------------------------------------------------------------------------

-- |Parse command-line option that represents compression method.
-- ������������ ������ ������ ������ � ���� ��������� ������, ��������� ��� � ������ ����������
-- "��� ����� -> ����� ������". ������ ������� ����� ������ ��������� ����� ������ �� ���������
decode_method configuredMethodSubsts str =
    str                       -- "3/$obj=2b/$iso=ecm+3b"
    .$ subst list             -- "3b/3t/$obj=2b/$iso=ecm+3b"
    .$ split_to_methods       -- [("","exe+3b"), ("$obj","3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ keepOnlyLastOn fst     -- [("","exe+3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ filter (not.null.snd)  -- "-m$bmp=" �������� ��������� ������������� ������������ ��������� ��� ������ $bmp
    .$ mapSnds (subst2 list)  -- [("",["exe","lzma"]), ("$text",["ppmd"]), ("$obj",["lzma"]), ("$iso",["ecm","lzma"])]

    where list = prepareSubsts (concatMap reorder [configuredMethodSubsts, builtinMethodSubsts])   -- ������� ���������������� ������, ����� ����������, ����� ���� ������ ���������
          reorder list = a++b  where (a,b) = partition (notElem '#') list                          -- ������ ���� �����: ������� �������, �� ���������� #, ����� � # (������� ����������, ����� ����� ������)

-- ������ �� ������ ��� ������ ������ (����������� ����������� ��� ������ ���� �����)
subst list method  =  joinWith "/" (main_methods:group_methods++user_methods)
  where -- �� ������ ���� -m3/$obj=2b �������� ��� ����������� ������ ������ �����, �� �����
        main:user_methods = split '/' method
        -- ����������� �������� ������� ������, ���� 3x = 3xb/3xt
        main_methods = case (lookup main list) of
            Just x  -> subst list x   -- ��� ������ ��������� ����������
            Nothing -> main           -- ������ ����������� ���
        -- ����� � ������ ����������� �������������� ������ ������ ��� ��������� �����, ���� 3x$iso = ecm+exe+3xb
        group_methods = list .$ keepOnlyFirstOn fst                      -- ������ ��������� ����������� (�� ����� ���������� ������ ��� ������ �����, ���� �� ����� �������������)
                             .$ mapMaybe (startFrom main . join2 "=")    -- ������� ������ �����������, ������������ � 3x, ������ ��� 3x
                             .$ filter (("$"==).take 1)                  -- � �� ��� - ������ ������������ � $

-- ������ �� ������ ��� ��������� ������ (����-�� ������������ ��� ����������� ���� ������)
subst2 list  =  concatMap f . split_compressor
    where f method = let (head,params)  =  break (==':') method
                     in case (lookup head list) of
                          Just new_head -> subst2 list (new_head++params)
                          Nothing       -> [decode_one_method method]

-- |������������ ���� ��������� ����� ������.
decode_one_method method | isFakeMethod method = method
                         | otherwise           = CompressionLib.canonizeCompressionMethod method

-- ���������� ������� ������, ����������� ������ ������ ��� ������ ����� ������,
-- � ������ ���������� (��� �����, ����� ������)
split_to_methods method = case (split '/' method) of
    [_]                 ->  [("",method)]   -- ���� ����� ��� ������ ���� �����
    x : xs@(('$':_):_) ->  ("",x) : map (split2 '=') xs   -- m1/$type=m2...
    b : t : xs          ->  [("","exe+"++b), ("$obj",b), ("$text",t)] ++ map (split2 '=') xs   -- m1/m2/$type=m3...

-- ����������� ������ ����� � ������������� � lookup
prepareSubsts x = x
    -- ������� ������ ������, ������� � �����������
    .$ map (filter (not.isSpace) . fst . split2 ';') .$ filter (not.null)
    -- �������� ������ ������ � �������� # �� 9 �����, ��� # ��������� �������� �� 1 �� 9
    .$ concatMap (\s -> if s `contains` '#'  then map (\d->replace '#' d s) ['1'..'9']  else [s])
    -- ������������� ������ ����� ���� "a=b" � ������ ��� lookup
    .$ map (split2 '=')

-- ���������� �������� ������� ������ � �������, ����������� ������������� � arc.ini
builtinMethodSubsts = [
      ";High-level method definitions"
    , "x  = 9            ;highest compression mode using only internal algorithms"
    , "ax = 9p           ;highest compression mode involving external compressors"
    , "0  = storing"
    , "1  = 1b  / $exe=exe+1b"
    , "1x = 1"
    , "#  = #rep+exe+#xb / $obj=#b / $text=#t"
    , "#x = #xb/#xt"
    , ""
    , ";Text files compression with slow decompression"
    , "1t  = 1b"
    , "2t  = grzip:m4:8m:32:h15"
    , "3t  = dict:p: 64m:85% + lzp: 64m: 24:h20        :92% + grzip:m3:8m:l"
    , "4t  = dict:p: 64m:80% + lzp: 64m: 65:d1m:s16:h20:90% + ppmd:8:96m"
    , "5t  = dict:p: 64m:80% + lzp: 80m:105:d1m:s32:h22:92% + ppmd:12:192m"
    , "6t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "7t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "8t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "9t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , ""
    , ";Binary files compression with slow and/or memory-expensive decompression"
    , "1b  = 1xb"
    , "#b  = #rep+#bx"
    , "2rep  = rep:  96m"
    , "3rep  = rep:  96m"
    , "4rep  = rep:  96m"
    , "5rep  = rep: 128m"
    , "6rep  = rep: 256m"
    , "7rep  = rep: 512m"
    , "8rep  = rep:1024m"
    , "9rep  = rep:2040m"
    , ""
    , ";Text files compression with fast decompression"
    , "1xt = 1xb"
    , "2xt = 2xb"
    , "3xt = dict:  64m:80% + tor:7:96m:h64m"
    , "4xt = dict:  64m:75% + 4binary"
    , "#xt = dict: 128m:75% + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1xb = tor:3"
    , "2xb = tor:96m:h64m"
    , "#xb = delta + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1binary = tor:3"
    , "2binary = tor:  96m:h64m"
    , "3binary = lzma: 96m:fast  :mc8"
    , "4binary = lzma: 96m:normal:mc16"
    , "5binary = lzma: 16m:max"
    , "6binary = lzma: 32m:max"
    , "7binary = lzma: 64m:max"
    , "8binary = lzma:128m:max"
    , "9binary = lzma:255m:max"
    , ""
    , ";Synonyms"
    , "bcj = exe"
    , "#bx = #xb"
    , "#tx = #xt"
    , "x#  = #x"    -- ��������� ����� ���� "-mx7" ��� �������� ��� 7-zip
    , ""
    , ";Compression modes involving external PPMONSTR.EXE"
    , "#p  = #rep+exe+#xb / $obj=#pb / $text=#pt"
    , "5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0"
    , "6pt = dict:p: 64m:80% + lzp: 64m:64:h22:85% + pmm:16:384m:r1"
    , "7pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:20:768m:r1"
    , "8pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:24:1536m:r1"
    , "9pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:25:2040m:r1"
    , "#pt = #t"
    , "#pb = #b"
    , ""
    , "#q  = #qb/#qt"
    , "5qt = dict:p:64m:80% + lzp:64m:64:d1m:24:h22:85% + pmm:10:160m:r1"
    , "5qb = rep: 128m      + delta                     + pmm:16:160m:r1"
    , "6qb = rep: 256m      + delta                     + pmm:20:384m:r1"
    , "7qb = rep: 512m      + delta                     + pmm:22:768m:r1"
    , "8qb = rep:1024m      + delta                     + pmm:24:1536m:r1"
    , "9qb = rep:2040m      + delta                     + pmm:25:2040m:r1"
    , "#qt = #pt"
    , "#qb = #pb"
    , ""
    , ";Sound wave files are compressed best with TTA"
    , "wav     = tta      ;best compression"
    , "wavfast = tta:m1   ;faster compression and decompression"
    , "1$wav  = wavfast"
    , "2$wav  = wavfast"
    , "#$wav  = wav"
    , "#x$wav = wavfast"
    , "#p$wav = wav"
    , ""
    , ";Bitmap graphic files are compressed best with GRZip"
    , "bmp        = mm    + grzip:m1:l2048:a  ;best compression"
    , "bmpfast    = mm    + grzip:m4:l:a      ;faster compression"
    , "bmpfastest = mm:d1 + tor:3:t0          ;fastest one"
    , "1$bmp  = bmpfastest"
    , "2$bmp  = bmpfastest"
    , "3$bmp  = bmpfast"
    , "#$bmp  = bmp"
    , "1x$bmp = bmpfastest"
    , "2x$bmp = bmpfastest"
    , "#x$bmp = mm+#binary"
    , "#p$bmp = bmp"
    , ""
    , ";Quick & dirty compression for data already compressed"
    , "4$compressed   = rep:96m + tor:c3"
    , "3$compressed   = rep:96m + tor:3"
    , "2$compressed   = rep:96m + tor:3"
    , "4x$compressed  = tor:8m:c3"
    , "3x$compressed  = rep:8m  + tor:3"
    , "2x$compressed  = rep:8m  + tor:3"
    ]

-- |�������������� ��� ������?
isMMType x  =  x `elem` words "$wav $bmp"

-- |� ��������� ������ �������� �������� - ���������� ���� ����� �� ��� �����������
typeByCompressor c  =  case (map method_name c) of
  xs | xs `contains` "tta"        -> "$wav"
     | xs `contains` "mm"         -> "$bmp"
     | xs `contains` "grzip"      -> "$text"
     | xs `contains` "ppmd"       -> "$text"
     | xs `contains` "pmm"        -> "$text"
     | xs `contains` "dict"       -> "$text"
     | xs == aNO_COMPRESSION      -> "$compressed"
     | xs == ["rep","tor"]        -> "$compressed"
     | xs `contains` "ecm"        -> "$iso"
     | xs `contains` "precomp"    -> "$precomp"
     | xs == ["precomp","rep"]    -> "$jpgsolid"
     | xs `contains` "jpg"        -> "$jpg"
     | xs `contains` "exe"        -> "$binary"
     | xs `contains` "lzma"       -> "$obj"
     | xs `contains` "tor"        -> "$obj"
     | otherwise                  -> "default"

-- |������ ���� ����� ������, �������������� �������� �������
typesByCompressor = words "$wav $bmp $text $compressed $iso $precomp $jpgsolid $jpg $obj $binary $exe"


-- |Human-readable description of compression method
encode_method uc  =  joinWith ", " (map encode_one_method uc)
encode_one_method (group,compressor)  =  between group " => " (join_compressor compressor)
join_compressor   =  joinWith "+"

-- |Opposite to join_compressor (used to read compression method from archive file)
split_compressor  =  split '+'

-- |���������� ��������� � ����������� ������������ ��������� process
process_algorithms process compressor = do
    return (split_compressor compressor)
       >>=  mapM process
       >>== join_compressor

-- |������� ����� ������ �� ��������� � ��������� ���������
split_method = split ':'

-- |��� ������ ������.
method_name = head . split_method

-- |������, ������������� ������������ �� ������������ ������ ������
showMem 0      = "0b"
showMem mem    = showM [(gb,"gb"),(mb,"mb"),(kb,"kb"),(b,"b"),error"showMem"] mem

showMemory 0   = "0 bytes"
showMemory mem = showM [(gb," gbytes"),(mb," mbytes"),(kb," kbytes"),(b," bytes"),error"showMemory"] mem

showM xs@( (val,str) : ~(nextval,_) : _) mem =
  if mem `mod` val==0 || mem `div` nextval>=4096
    then show((mem+val`div` 2) `div` val)++str
    else showM (tail xs) mem

-- |��������� ����� ������ ����� ���, ����� �� ������� �������������
roundMemUp mem | mem>=4096*kb = mem `roundUp` mb
               | otherwise    = mem `roundUp` kb

{-# NOINLINE builtinMethodSubsts #-}
{-# NOINLINE decode_method #-}
{-# NOINLINE showMem #-}

