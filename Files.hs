{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- �������� � ������� ������, ����������� � ������� �� �����, ����/�����.                     ----
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  Files
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Files (module Files, module FilePath) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Array
import Data.Char
import Data.IORef
import Data.List
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.Posix.Internals hiding (CFilePath)
import System.Posix.Types
import System.IO
import System.IO.Error hiding (catch)
import System.IO.Unsafe
import System.Environment
import System.Locale
import System.Time
import System.Process
import System.Directory

import Utils
import FilePath
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32
#else
import System.Posix.Files hiding (fileExist)
#endif

-- |������ ������ ������, ������������ � ��������� ���������
aBUFFER_SIZE = 64*kb

-- |���������� ����, ������� ������ ��������/������������ �� ���� ��� � ������� ������� � ��� ���������� ������������� ����������
aLARGE_BUFFER_SIZE = 256*kb

-- |���������� ����, ������� ������ ��������/������������ �� ���� ��� � ����� ������� ������� (storing, tornado � ���� ��������)
-- ���� ����� ������������ ������ �� disk seek operations - ��� �������, ��� ������������ �� ���������� �/� � ������ ������ ;)
aHUGE_BUFFER_SIZE = 8*mb


----------------------------------------------------------------------------------------------------
---- Filename manipulations ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |True, ���� file ��������� � �������� `dir`, ����� �� ��� ������������, ��� ��������� � ���
dir `isParentDirOf` file =
  case (startFrom dir file) of
    Just ""    -> True
    Just (x:_) -> isPathSeparator x
    Nothing    -> False

-- |��� ����� �� ������� �������� dir
file `dropParentDir` dir =
  case (startFrom dir file) of
    Just ""    -> ""
    Just (x:xs) | isPathSeparator x -> xs
    _          -> error "Utils::dropParentDir: dir isn't prefix of file"


#if defined(FREEARC_WIN)
-- |��� case-insensitive �������� ������
filenameLower = strLower
#else
-- |��� case-sensitive �������� ������
filenameLower = id
#endif

-- |Return False for special filenames like "." and ".." - used to filtering results of getDirContents
exclude_special_names s  =  (s/=".")  &&  (s/="..")

-- Strip "drive:/" at the beginning of absolute filename
stripRoot = dropDrive

-- |Replace all '\' with '/'
translatePath = map (\c -> if isPathSeparator c  then '/'  else c)

-- |Filename extension, "dir/name.ext" -> "ext"
getFileSuffix = snd . splitFilenameSuffix

splitFilenameSuffix str  =  (name, drop 1 ext)
                               where (name, ext) = splitExtension str

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitDirFilename :: String -> (String,String)
splitDirFilename str  =  case splitFileName str of
                           x@([d,':',s], name) -> x   -- ��������� ("c:\", name)
                           (dir, name)         -> (dropTrailingPathSeparator dir, name)

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", "ext")
splitFilename3 :: String -> (String,String,String)
splitFilename3 str
   = let (dir, rest) = splitDirFilename str
         (name, ext) = splitFilenameSuffix rest
     in  (dir, name, ext)

-- | Modify the base name.
updateBaseName :: (String->String) -> FilePath -> FilePath
updateBaseName f pth  =  dir </> f name <.> ext
    where
          (dir, name, ext) = splitFilename3 pth


----------------------------------------------------------------------------------------------------
---- ����� ������-������ ��������� � SFX ������� ---------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |����� ������-���� � �������� ������ ��� ���������� ""
findFile = findName fileExist
findDir  = findName dirExist
findName exist possibleFilePlaces cfgfilename = do
  found <- possibleFilePlaces cfgfilename >>= Utils.filterM exist
  case found of
    x:xs -> return x
    []   -> return ""

-- |����� ������-���� � �������� ������ ��� ���������� ��� ��� �������� ������ �����
findOrCreateFile possibleFilePlaces cfgfilename = do
  variants <- possibleFilePlaces cfgfilename
  found    <- Utils.filterM fileExist variants
  case found of
    x:xs -> return x
    []   -> return (head variants)


#if defined(FREEARC_WIN)
-- ��� Windows ��� �������������� ����� �� ��������� ����� � ����� �������� � ����������
libraryFilePlaces = configFilePlaces
configFilePlaces filename  =  do -- dir1 <- getAppUserDataDirectory "FreeArc"
                                 exe  <- getExeName
                                 return [-- dir1              </> filename,
                                         takeDirectory exe </> filename]

-- |��� ������������ ����� ���������
getExeName = do
  allocaBytes (long_path_size*4) $ \pOutPath -> do
    c_GetExeName pOutPath (fromIntegral long_path_size*2) >>= peekCWString

foreign import ccall unsafe "Environment.h GetExeName"
  c_GetExeName :: CWFilePath -> CInt -> IO CWFilePath

#else
-- |����� ��� ������ ������-������
configFilePlaces  filename  =  do dir1 <- getAppUserDataDirectory "FreeArc"
                                  return [dir1   </> filename
                                         ,"/etc/FreeArc" </> filename]

-- |����� ��� ������ sfx-�������
libraryFilePlaces filename  =  return ["/usr/lib/FreeArc"       </> filename
                                      ,"/usr/local/lib/FreeArc" </> filename]

-- |��� ������������ ����� ���������
getExeName = getProgName
#endif


-- |Get temporary files directory
getTempDir = c_GetTempDir >>= peekCFilePath

foreign import ccall safe "Environment.h GetTempDir"
  c_GetTempDir :: IO CFilePath

-- |Set directory for temporary files
setTempDir dir = withCFilePath dir c_SetTempDir

foreign import ccall safe "Environment.h SetTempDir"
  c_SetTempDir :: CFilePath -> IO ()


----------------------------------------------------------------------------------------------------
---- ������ ������� �������� � ������ � Windows registry -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |��������� ������� ����� shell � ���������� � stdout
runProgram cmd = do
    (_, stdout, stderr, ph) <- runInteractiveCommand cmd
    forkIO (hGetContents stderr >>= evaluate.length >> return ())
    result <- hGetContents stdout
    evaluate (length result)
    waitForProcess ph
    return result

-- |Execute file/command in the directory `curdir` optionally waiting until it finished
runFile    = runIt c_RunFile
runCommand = runIt c_RunCommand
runIt :: (CFilePath -> CFilePath -> CInt -> IO ()) -> String -> String -> Bool -> IO ()
runIt c_run_it filename curdir wait_finish = do
  withCFilePath filename $ \c_filename -> do
    withCFilePath curdir   $ \c_curdir   -> do
      c_run_it c_filename c_curdir (i$fromEnum wait_finish)

foreign import ccall safe "Environment.h RunFile"
  c_RunFile :: CFilePath -> CFilePath -> CInt -> IO ()

foreign import ccall safe "Environment.h RunCommand"
  c_RunCommand :: CFilePath -> CFilePath -> CInt -> IO ()

-- |��������� ������ ������� �� ������ ����� ����������
unparseCommand  =  joinWith " " . map quote


#if defined(FREEARC_WIN)
-- |������� HKEY � ��������� �� Registry �������� ���� REG_SZ
registryGetStr root branch key =
  bracket (regOpenKey root branch) regCloseKey
    (\hk -> registryGetStringValue hk key)

-- |������� HKEY � �������� � Registry �������� ���� REG_SZ
registrySetStr root branch key val =
  bracket (regCreateKey root branch) regCloseKey
    (\hk -> registrySetStringValue hk key val)

-- |��������� �� Registry �������� ���� REG_SZ
registryGetStringValue :: HKEY -> String -> IO (Maybe String)
registryGetStringValue hk key = do
  (regQueryValue hk (Just key) >>== Just)
    `catch` (\e -> return Nothing)

-- |�������� � Registry �������� ���� REG_SZ
registrySetStringValue :: HKEY -> String -> String -> IO ()
registrySetStringValue hk key val =
  withTString val $ \v ->
  regSetValueEx hk key rEG_SZ v (length val*2)

-- |������� ����� ����� �� Registry
registryDeleteTree :: HKEY -> String -> IO ()
registryDeleteTree key subkey = do
  handle (\e -> return ()) $ do
  withForeignPtr key $ \ p_key -> do
  withTString subkey $ \ c_subkey -> do
  failUnlessSuccess "registryDeleteTree" $ c_RegistryDeleteTree p_key c_subkey
foreign import ccall unsafe "Environment.h RegistryDeleteTree"
  c_RegistryDeleteTree :: PKEY -> LPCTSTR -> IO ErrCode
#endif


#if defined(FREEARC_WIN)
-- |OS-specific thread id
foreign import stdcall unsafe "windows.h GetCurrentThreadId"
  getOsThreadId :: IO DWORD
#else
foreign import stdcall unsafe "pthread.h pthread_self"
  getOsThreadId :: IO Int
#endif


----------------------------------------------------------------------------------------------------
---- �������� � ����������� ������� � ���������� ---------------------------------------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)
-- |������ ������ � ������� � �� ������
getDrives = getLogicalDrives >>== unfoldr (\n -> Just (n `mod` 2, n `div` 2))
                             >>== zipWith (\c n -> n>0 &&& [c:":"]) ['A'..'Z']
                             >>== concat
                             >>=  mapM (\d -> do t <- withCString d c_GetDriveType; return (d++"\t"++(driveTypes!!i t)))

driveTypes = (split ',' "???,???,Removable,Fixed,Network,CD/DVD,Ramdisk") ++ repeat "???"

foreign import stdcall unsafe "windows.h GetDriveTypeA"
  c_GetDriveType :: LPCSTR -> IO CInt
#endif


-- |Create a hierarchy of directories
createDirectoryHierarchy :: FilePath -> IO ()
createDirectoryHierarchy dir0 = do
  let dir = dropTrailingPathSeparator dir0
      d   = stripRoot dir
  when (d/= "" && exclude_special_names d) $ do
    unlessM (dirExist dir) $ do
      createDirectoryHierarchy (takeDirectory dir)
      dirCreate dir

-- |������� ����������� �������� �� ���� � �����
buildPathTo filename  =  createDirectoryHierarchy (takeDirectory filename)

-- |Return current directory
getCurrentDirectory = myCanonicalizePath "."

-- | Given path referring to a file or directory, returns a
-- canonicalized path, with the intent that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path. Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fpath | isURL fpath = return fpath
                         | otherwise   =
#if defined(FREEARC_WIN)
  withCFilePath fpath $ \pInPath ->
  allocaBytes (long_path_size*4) $ \pOutPath ->
  alloca $ \ppFilePart ->
    do c_GetFullPathName pInPath (fromIntegral long_path_size*2) pOutPath ppFilePart
       peekCFilePath pOutPath >>== dropTrailingPathSeparator

foreign import stdcall unsafe "GetFullPathNameW"
            c_GetFullPathName :: CWString
                              -> CInt
                              -> CWString
                              -> Ptr CWString
                              -> IO CInt
#else
  withCFilePath fpath $ \pInPath ->
  allocaBytes (long_path_size*4) $ \pOutPath ->
    do c_realpath pInPath pOutPath
       peekCFilePath pOutPath >>== dropTrailingPathSeparator

foreign import ccall unsafe "realpath"
                   c_realpath :: CString
                              -> CString
                              -> IO CString
#endif

-- |������������ ����� ����� �����
long_path_size  =  i c_long_path_size :: Int
foreign import ccall unsafe "Environment.h long_path_size"
  c_long_path_size :: CInt


#if defined(FREEARC_WIN)
-- |Clear file's Archive bit
clearArchiveBit filename = do
    attr <- getFileAttributes filename
    when (attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0) $ do
        setFileAttributes filename (attr - fILE_ATTRIBUTE_ARCHIVE)
-- |Clear all file's attributes (before deletion)
clearFileAttributes filename = do
    setFileAttributes filename 0
#else
clearArchiveBit _    = return ()
clearFileAttributes _ = return ()
#endif


-- |����������� datetime, ������� ������ ����� ���� � �����. ������������� 1 ������ 1970 �.
aMINIMAL_POSSIBLE_DATETIME = 0 :: CTime

-- |Get file's date/time
getFileDateTime filename  =  fileWithStatus "getFileDateTime" filename stat_mtime

-- |Set file's date/time
setFileDateTime filename datetime  =  withCFilePath filename (`c_SetFileDateTime` datetime)

foreign import ccall unsafe "Environment.h SetFileDateTime"
   c_SetFileDateTime :: CFilePath -> CTime -> IO ()

-- |������������� CTime � ClockTime. ������������ ���������� � ���������� ������������� ClockTime � GHC!!!
convert_CTime_to_ClockTime ctime = TOD (realToInteger ctime) 0
  where realToInteger = round . realToFrac :: Real a => a -> Integer

-- |������������� ClockTime � CTime
convert_ClockTime_to_CTime (TOD secs _) = i secs

-- |��������� ������������� �������
showtime format t = formatCalendarTime defaultTimeLocale format (unsafePerformIO (toCalendarTime t))

-- |��������������� CTime � ������ � �������� "%Y-%m-%d %H:%M:%S"
formatDateTime t  =  unsafePerformIO $ do
  allocaBytes 100 $ \buf -> do
    c_FormatDateTime buf 100 t
    peekCString buf

foreign import ccall unsafe "Environment.h FormatDateTime"
  c_FormatDateTime :: CString -> CInt -> CTime -> IO ()


#if defined(FREEARC_UNIX)
executeModes         =  [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
removeFileModes a b  =  a `intersectFileModes` (complement b)
#endif

-- Wait a few seconds (no more than half-hour due to Int overflow!)
sleepSeconds secs = do let us = round (secs*1000000)
                       threadDelay us


----------------------------------------------------------------------------------------------------
---- �������� � ��������� ������� ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--withMVar  mvar action     =  bracket (takeMVar mvar) (putMVar mvar) action
liftMVar1  action mvar     =  withMVar mvar action
liftMVar2  action mvar x   =  withMVar mvar (\a -> action a x)
liftMVar3  action mvar x y =  withMVar mvar (\a -> action a x y)
returnMVar action          =  action >>= newMVar

-- |�������� ����, �������������� � MVar ��� ���������� ������������� ������� �� ������ ������ �� ������� �������
data Archive = Archive { archiveName :: FilePath
                       , archiveFile :: MVar File
                       }
archiveOpen     name = do file <- fileOpen name >>= newMVar; return (Archive name file)
archiveCreate   name = do file <- fileCreate name >>= newMVar; return (Archive name file)
archiveCreateRW name = do file <- fileCreateRW name >>= newMVar; return (Archive name file)
archiveGetPos        = liftMVar1 fileGetPos   . archiveFile
archiveGetSize       = liftMVar1 fileGetSize  . archiveFile
archiveSeek          = liftMVar2 fileSeek     . archiveFile
archiveRead          = liftMVar2 fileRead     . archiveFile
archiveReadBuf       = liftMVar3 fileReadBuf  . archiveFile
archiveWrite         = liftMVar2 fileWrite    . archiveFile
archiveWriteBuf      = liftMVar3 fileWriteBuf . archiveFile
archiveClose         = liftMVar1 fileClose    . archiveFile

-- |����������� ������ �� ������ ������ � ������ � ����� ������������ ������� � �������� ������
archiveCopyData srcarc pos size dstarc = do
  withMVar (archiveFile srcarc) $ \srcfile ->
    withMVar (archiveFile dstarc) $ \dstfile -> do
      restorePos <- fileGetPos srcfile
      fileSeek      srcfile pos
      fileCopyBytes srcfile size dstfile
      fileSeek      srcfile restorePos

-- |��� ������ � ����� ���������� ������ (�������� ������ �������)
-- ��� ������ ��������� ��������� I/O �������� �����������,
-- ������� �� �� ��� �������� ����� "�������� ����" �����-������������ MVar
oneIOAtTime = unsafePerformIO$ newMVar "oneIOAtTime value"
fileReadBuf  file buf size = withMVar oneIOAtTime $ \_ -> fileReadBufSimple  file buf size
fileWriteBuf file buf size = withMVar oneIOAtTime $ \_ -> fileWriteBufSimple file buf size


----------------------------------------------------------------------------------------------------
---- URL access ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data File = FileOnDisk FileOnDisk | URL URL

fileOpen           = choose0 fOpen           url_open
fileCreate         = choose0 fCreate         (\_ -> err "url_create")
fileCreateRW       = choose0 fCreateRW       (\_ -> err "url_create_rw")
fileAppendText     = choose0 fAppendText     (\_ -> err "url_append_text")
fileGetPos         = choose  fGetPos         (url_pos  .>>==i)
fileGetSize        = choose  fGetSize        (url_size .>>==i)
fileSeek           = choose  fSeek           (\f p -> url_seek f (i p))
fileReadBufSimple  = choose  fReadBufSimple  url_read
fileWriteBufSimple = choose  fWriteBufSimple (\_ _ _ -> err "url_write")
fileFlush          = choose  fFlush          (\_     -> err "url_flush")
fileClose          = choose  fClose          url_close

-- |��������� ������������� �����/URL
fileExist name | isURL name = do url <- withCString name url_open
                                 url_close url
                                 return (url/=nullPtr)
               | otherwise  = fExist name

-- |���������, �������� �� ��� url
isURL name = "://" `isInfixOf` name

{-# NOINLINE choose0 #-}
choose0 onfile onurl name | isURL name = do url <- withCString name onurl
                                            when (url==nullPtr) $ do
                                              fail$ "Can't open url "++name   --registerError$ CANT_OPEN_FILE name
                                            return (URL url)
                          | otherwise  = onfile name >>== FileOnDisk

choose _ onurl  (URL        url)   = onurl  url
choose onfile _ (FileOnDisk file)  = onfile file

{-# NOINLINE err #-}
err s  =  fail$ s++" isn't implemented"    --registerError$ GENERAL_ERROR ["0343 %1 isn't implemented", s]


type URL = Ptr ()
foreign import ccall safe "URL.h"  url_setup_proxy         :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_setup_bypass_list   :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_open   :: Ptr CChar -> IO URL
foreign import ccall safe "URL.h"  url_pos    :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_size   :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_seek   :: URL -> Int64 -> IO ()
foreign import ccall safe "URL.h"  url_read   :: URL -> Ptr a -> Int -> IO Int
foreign import ccall safe "URL.h"  url_close  :: URL -> IO ()


----------------------------------------------------------------------------------------------------
---- ��� Windows ��� �������� ����������� ���������� �/� ������ ��� ��������� ������ >4Gb � Unicode ��� ������
----------------------------------------------------------------------------------------------------
#if defined(FREEARC_WIN)

type FileOnDisk      = FD
type CFilePath       = CWFilePath
type FileAttributes  = FileAttributeOrFlag
withCFilePath        = withCWFilePath
peekCFilePath        = peekCWString
fOpen       name     = wopen name (read_flags  .|. o_BINARY) 0o666
fCreate     name     = wopen name (write_flags .|. o_BINARY .|. o_TRUNC) 0o666
fCreateRW   name     = wopen name (rw_flags    .|. o_BINARY .|. o_TRUNC) 0o666
fAppendText name     = wopen name (append_flags) 0o666
fGetPos              = wtell
fGetSize             = wfilelength
fSeek   file pos     = wseek file pos sEEK_SET
fReadBufSimple       = wread
fWriteBufSimple      = wwrite
fFlush  file         = return ()
fClose               = wclose
fExist               = wDoesFileExist
fileRemove           = wunlink
fileRename           = wrename
fileWithStatus       = wWithFileStatus
fileStdin            = 0
stat_mode            = wst_mode
stat_size            = wst_size
stat_mtime           = wst_mtime
dirCreate            = wmkdir
dirExist             = wDoesDirectoryExist
dirRemove            = wrmdir
dirList dir          = dirWildcardList (dir </> "*")
dirWildcardList wc   = withList $ \list -> do
                         wfindfiles wc $ \find -> do
                           name <- w_find_name find
                           list <<= name

#else

type FileOnDisk      = Handle
type CFilePath       = CString
type FileAttributes  = Int
withCFilePath s a    = (`withCString` a) =<< str2filesystem s
peekCFilePath ptr    = peekCString ptr >>= filesystem2str
fOpen                = (`openBinaryFile` ReadMode     ) =<<. str2filesystem
fCreate              = (`openBinaryFile` WriteMode    ) =<<. str2filesystem
fCreateRW            = (`openBinaryFile` ReadWriteMode) =<<. str2filesystem
fAppendText          = (`openFile`       AppendMode   ) =<<. str2filesystem
fGetPos              = hTell
fGetSize             = hFileSize
fSeek                = (`hSeek` AbsoluteSeek)
fReadBufSimple       = hGetBuf
fWriteBufSimple      = hPutBuf
fFlush               = hFlush
fClose               = hClose
fExist               = doesFileExist =<<. str2filesystem
fileGetStatus        = getFileStatus =<<. str2filesystem
fileSetMode name mode= (`setFileMode` mode) =<< str2filesystem name
fileRemove name      = removeFile    =<<  str2filesystem name
fileRename a b       = do a1 <- str2filesystem a; b1 <- str2filesystem b; renameFile a1 b1
fileSetSize          = hSetFileSize
fileStdin            = stdin
stat_mode            = st_mode
stat_size            = st_size  .>>== i
stat_mtime           = st_mtime
dirCreate            = createDirectory     =<<. str2filesystem
dirExist             = doesDirectoryExist  =<<. str2filesystem
dirRemove            = removeDirectory     =<<. str2filesystem
dirList dir          = str2filesystem dir >>= getDirectoryContents >>= mapM filesystem2str
dirWildcardList wc   = dirList (takeDirectory wc)  >>==  filter (match$ takeFileName wc)

-- kidnapped from System.Directory :)))
fileWithStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
fileWithStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
	f p

#endif

fileRead      file size = allocaBytes size $ \buf -> do fileReadBuf file buf size; peekCStringLen (buf,size)
fileWrite     file str  = withCStringLen str $ \(buf,size) -> fileWriteBuf file buf size
fileGetBinary name      = bracket (fileOpen   name) fileClose (\file -> fileGetSize file >>= fileRead file.i)
filePutBinary name str  = bracket (fileCreate name) fileClose (`fileWrite` str)

-- |����������� �������� ���������� ���� �� ������ ��������� ����� � ������
fileCopyBytes srcfile size dstfile = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do        -- ���������� `alloca`, ����� ������������� ���������� ���������� ����� ��� ������
    doChunks size aHUGE_BUFFER_SIZE $ \bytes -> do  -- ����������� size ���� ������� �� aHUGE_BUFFER_SIZE
      bytes <- fileReadBuf srcfile buf bytes        -- ��������, ��� ��������� ����� ������� ����, ������� �����������
      fileWriteBuf dstfile buf bytes

-- |True, ���� ���������� ���� ��� ������� � �������� ������
fileOrDirExist f  =  mapM ($f) [fileExist, dirExist] >>== or


---------------------------------------------------------------------------------------------------
---- ���������� ��������� ������������� ��� ������������� � ������� ��������� �������� ------------
---------------------------------------------------------------------------------------------------

-- |Translate filename from filesystem to internal encoding
-- On GHC 9+, getDirectoryContents/peekCString already return proper Unicode,
-- so filesystem encoding is handled by the runtime. These are identity functions.
filesystem2str'   = unsafePerformIO$ newIORef$ id
filesystem2str s  = val filesystem2str' >>== ($s)
-- |Translate filename from internal to filesystem encoding
str2filesystem'   = unsafePerformIO$ newIORef$ id
str2filesystem s  = val str2filesystem' >>== ($s)


---------------------------------------------------------------------------------------------------
---- Utility functions ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

foreign import ccall unsafe "string.h"
    memset :: Ptr a -> Int -> CSize -> IO ()

foreign import ccall unsafe "Environment.h memxor"
    memxor :: Ptr a -> Ptr a -> Int -> IO ()

