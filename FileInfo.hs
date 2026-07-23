{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- ��������� � �������� ���������� � ������, ����� ������ �� �����.                           ----
----------------------------------------------------------------------------------------------------
module FileInfo where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Char
#ifdef FREEARC_PACKED_STRINGS
import Data.HashTable as Hash
#endif
import Data.Int
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import System.IO.Unsafe
import System.Posix.Internals

import Utils
import Process
import Files
import Errors
#ifdef FREEARC_PACKED_STRINGS
import UTF8Z
#endif
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32.File
#endif


----------------------------------------------------------------------------------------------------
---- ���������� ������������� ����� ����� ----------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ��� ������ � ���������� ���� � �������������� �������� �������
-- � ����� ��������, ����� ����� ��� �������� � ���������� �����
data PackedFilePath = PackedFilePath
  { fpPackedDirectory       :: !MyPackedString     -- ��� ��������
  , fpPackedBasename        :: !MyPackedString     -- ��� ����� ��� ��������, �� � �����������
  , fpLCExtension           :: !String             -- ����������, ����������� � ������ �������
  , fpHash   :: {-# UNPACK #-} !Int32              -- ��� �� ����� �����
  , fpParent                :: !PackedFilePath     -- ��������� PackedFilePath ������������� ��������
  }
  | RootDir

instance Eq PackedFilePath where
  (==)  =  map2eq$ map3 (fpHash,fpPackedBasename,fpPackedDirectory)

#ifdef FREEARC_PACKED_STRINGS
-- ������������� ����������� ����� ��������� ������ ������ � 2 ����
type MyPackedString = PackedString
myPackStr           = packString
myUnpackStr         = unpackPS

-- |�������� ���������� ���������� ���������� ����� � ��� �� �������
packext ext = unsafePerformIO$ do
  found <- Hash.lookup extsHash ext
  case found of
    Nothing      -> do Hash.insert extsHash ext ext
                       return ext
    Just oldext  -> return oldext

extsHash = unsafePerformIO$ Hash.new (==) (filenameHash 0)

#else
type MyPackedString = String
myPackStr           = id
myUnpackStr         = id
packext             = id
#endif

fpDirectory  =  myUnpackStr.fpPackedDirectory
fpBasename   =  myUnpackStr.fpPackedBasename

-- |����������� ����: ������ ��� �����, ������� ������� � ����������
fpFullname fp  =  fpDirectory fp </> fpBasename fp

-- |���������� ���������� ������������ ������� �����
fpPackedFullname fp  =  if fpPackedDirectory fp == myPackStr ""
                          then fpPackedBasename fp
                          else myPackStr (fpFullname fp)


-- |�������� ������������ ������������� �� ����� �����
packFilePath parent fullname  =  packFilePath2 parent dir name
  where (dir,name) = splitDirFilename fullname

-- |�������� ������������ ������������� �� ����� �������� � ����� ����� ��� ��������
packFilePath2       parent dir        name  =  packFilePathPacked2 parent (myPackStr dir) name
packFilePathPacked2 parent packed_dir name  =  packFilePathPacked3 parent packed_dir name (packext$ filenameLower$ getFileSuffix name)

-- |�������� ������������ ������������� �� ����� ��������, ����� ����� ��� �������� � ����������.
packFilePath3 parent dir name lcext              =  packFilePathPacked3 parent (myPackStr dir) name lcext
packFilePathPacked3 parent packed_dir name lcext =
  PackedFilePath { fpPackedDirectory    =  packed_dir
                 , fpPackedBasename     =  myPackStr name
                 , fpLCExtension        =  lcext
                 , fpHash               =  filenameHash (fpHash parent) name
                 , fpParent             =  parent
                 }

-- |������� ��������� ��� �������� �������� ��� ������ ������
packParentDirPath dir  =
  PackedFilePath { fpPackedDirectory    =  myPackStr ""   -- ����� �� ������� ��� �����,
                 , fpPackedBasename     =  myPackStr dir  -- �������� ��� �������� ������� � Basename
                 , fpLCExtension        =  ""
                 , fpHash               =  filenameHash 0 (filter (not.isPathSeparator) dir)
                 , fpParent             =  RootDir
                 }

-- |��� �� ������� ����� ����� (��� ������������ ��������!).
-- ��� ��������� ��� ���������� ������������ `dirhash` - ��� ����� ��������, ����������� ����,
-- � `basename` - ��� ����� ��� ����� ��������
filenameHash {-dirhash basename-}  =  foldl (\h c -> h*37+i(ord c))

{-# INLINE filenameHash #-}


----------------------------------------------------------------------------------------------------
---- ������������� ��� ������ � ����������� ����������� -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |����������� ��� ����� � ������ `filespec`.
-- ����� "*", "*.ext" ��� ��� ����� ��� �������� - �������������� �����
match_FP getName filespec =
  if filespec==reANY_FILE  then const True  else
    case (splitFilename3 filespec) of
      ("", "*", ext) -> match  (filenameLower ext)      . fpLCExtension
      ("", _,   _  ) -> match  (filenameLower filespec) . filenameLower . getName
      _              -> match  (filenameLower filespec) . filenameLower . fpFullname

-- |������������� �� ���� � ����� `filepath` ���� ����� �� ����� `filespecs`?
match_filespecs getName {-filespecs filepath-}  =  anyf . map (match_FP getName)

-- |�����, ������� ������������� ����� ��� �����
reANY_FILE = "*"


----------------------------------------------------------------------------------------------------
---- ���������� � ����� ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- ���� ������ ���..
type FileCount = Int              -- ���������� ������
type FileSize  = Integer          -- ������� ����� ��� ������� ������/������ � ���
aFILESIZE_MIN  = -(2^63)          -- ����� ��������� �������� ���� FileSize
type FileTime  = CTime            -- ������� ��������/�����������/������ �����
type FileAttr  = FileAttributes   -- ��������� ��������� �����
type FileGroup = Int              -- ������ ������ � arc.groups

-- |���������, �������� ��� ����������� ��� ���������� � �����
data FileInfo = FileInfo
  { fiFilteredName         :: !PackedFilePath  -- ��� �����, �������������� � ���������� � ��������� ������
  , fiDiskName             :: !PackedFilePath  -- "�������" ��� ����� - ��� ������/������ ������ �� �����
  , fiStoredName           :: !PackedFilePath  -- "����������" ��� ����� - ����������� � ���������� ������
  , fiSize  :: {-# UNPACK #-} !FileSize        -- ������ ����� (0 ��� ���������)
  , fiTime  :: {-# UNPACK #-} !FileTime        -- ����/����� �������� �����
  , fiAttr  :: {-# UNPACK #-} !FileAttr        -- ��������� �������� �����
  , fiIsDir :: {-# UNPACK #-} !Bool            -- ��� �������?
  , fiGroup :: {-# UNPACK #-} !FileGroup       -- ����� ������ � arc.groups
  }

-- |������������� FileInfo � ��� ����� �� �����
diskName     = fpFullname.fiDiskName
storedName   = fpFullname.fiStoredName
filteredName = fpFullname.fiFilteredName

-- |������������� FileInfo � ������� ��� �����
baseName     = fpBasename.fiStoredName

-- |����������� ����� (��������, �������� � ���� ��������) �� ������� ��������
fiSpecialFile = fiIsDir

-- |����� ������, ������������� ���, ��� �� �� ������������.
fiUndefinedGroup = -1

-- |������� ��������� FileInfo ��� �������� � �������� ������
createParentDirFileInfo fiFilteredName fiDiskName fiStoredName =
  FileInfo { fiFilteredName  =  packParentDirPath fiFilteredName
           , fiDiskName      =  packParentDirPath fiDiskName
           , fiStoredName    =  packParentDirPath fiStoredName
           , fiSize          =  0
           , fiTime          =  aMINIMAL_POSSIBLE_DATETIME
           , fiAttr          =  0
           , fiIsDir         =  True
           , fiGroup         =  fiUndefinedGroup
           }

-- |���������� ���������� � ����� ����� ��� �������� (�� ������, ���� ���� ����� ����������).
--  ���������� ������������ fiAttr (��� �������) � fiGroup
rereadFileInfo fi file = do
  getFileInfo (fiFilteredName fi) (fiDiskName fi) (fiStoredName fi)

-- |������� ��������� FileInfo � ����������� � �������� �����.
--  ���������� ������������ fiAttr (��� �������) � fiGroup
getFileInfo fiFilteredName fiDiskName fiStoredName  =
    let filename = fpFullname fiDiskName in do
    fileWithStatus "getFileInfo" filename $ \p_stat -> do
      fiIsDir  <-  stat_mode  p_stat  >>==  s_isdir
      fiTime   <-  stat_mtime p_stat
      fiSize   <-  if fiIsDir then return 0
                              else stat_size p_stat
      return$ Just$ FileInfo fiFilteredName fiDiskName fiStoredName fiSize fiTime 0 fiIsDir fiUndefinedGroup
  `catch` (\(_ :: SomeException) -> do registerWarning$ CANT_GET_FILEINFO filename
                                       return Nothing)  -- � ������ ������ ��� ���������� stat ���������� Nothing

-- |Restore date/time/attrs saved in FileInfo structure
setFileDateTimeAttr filename fileinfo  =  setFileDateTime filename (fiTime fileinfo)

{-# NOINLINE getFileInfo #-}


----------------------------------------------------------------------------------------------------
---- ������� ������ ������ �� ����� ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |��������� ��� �������� ������ ������ �� �����
data FindFiles = FindFiles
    { ff_disk_eq_filtered   :: Bool
    , ff_stored_eq_filtered :: Bool
    , ff_recursive          :: Bool
    , ff_parent_or_root     :: FileInfo -> FileInfo
    , ff_accept_f           :: FileInfo -> Bool
    , ff_process_f          :: [FileInfo] -> IO ()
    }


-- |������� FileInfo ������ � ��������� (�������� "." � ".."), ����������� � �������� `parent`
getDirectoryContents_FileInfo ff parent{-������������ ��������� FileInfo-} = do
  let -- ������ �������� ��� ���. ��������
      diskDirName = fpFullname$ fiDiskName parent
      -- ����������� ������ � ��������, ����������� � ������������ ������ ���. ��������
      -- ��� ����� ����� ��������� ��� ���������� -ap/-dp, ��� ��������� ��� ��������� ������ � ���� �������
      packedDisk  = myPackStr diskDirName
      packedFiltered = if ff.$ff_disk_eq_filtered
                          then packedDisk
                          else myPackStr$ fpFullname$ fiFilteredName parent
      packedStored   = if ff.$ff_stored_eq_filtered
                          then packedFiltered
                          else myPackStr$ fpFullname$ fiStoredName   parent_or_root
      -- ������� parent ��� root � �������� ������������ ������ (��������� - ������ ��� -ep0)
      parent_or_root = (ff.$ff_parent_or_root) parent

      -- ������� ������� f, ������� �� ������� ������������, ��������� � ������������ �����
      make_names f name = f (packFilePathPacked3 (fiFilteredName parent)          packedFiltered  name lcext)
                            (packFilePathPacked3 (fiDiskName     parent)          packedDisk      name lcext)
                            (packFilePathPacked3 (fiStoredName   parent_or_root)  packedStored    name lcext)
                          where lcext  =  packext$ filenameLower$ getFileSuffix name

#if !defined(FREEARC_WIN)
  (dirList (diskDirName|||".")) .$handleFindErrors diskDirName  -- ������� ������ ������ � ��������, ����������� ������ ������ ��������,
    >>== filter exclude_special_names                           -- �������� �� ������ "." � ".."
    >>= (mapMaybeM $! make_names getFileInfo)                   -- ��������� ����� ������ � ��������� FileInfo � ����� �� ������ �����, �� ������� ���������� `stat`
#else
  withList $ \list -> do
    handleFindErrors diskDirName $ do
      wfindfiles (diskDirName </> reANY_FILE) $ \find -> do
        name <- w_find_name find
        when (exclude_special_names name) $ do
          fiAttr  <- w_find_attrib     find
          fiSize  <- w_find_size       find
          fiTime  <- w_find_time_write find
          fiIsDir <- w_find_isDir      find
          (list <<=) $! make_names FileInfo name fiSize fiTime fiAttr fiIsDir fiUndefinedGroup
#endif


-- |�������� exception handler, ���������� ��� ������� ��������� ������ ������ � ��������
handleFindErrors dir =
  handle (\(_ :: IOException) -> do
    -- ��������� �� ������ �� ���������� ��� ��������� "/System Volume Information"
    d <- myCanonicalizePath dir
    unless (stripRoot d `strLowerEq` "System Volume Information") $ do
      registerWarning$ CANT_READ_DIRECTORY dir
    return defaultValue)

-- |������� ������ ������ � `dir`, ��������������� `accept_f` � �������� ��������� � `process_f`.
-- ���� recursive==True - ��������� ��� �������� ���������� � ������ ��������� �����������
findFiles_FileInfo dir ff@FindFiles{ff_accept_f=accept_f, ff_process_f=process_f, ff_recursive=recursive} = do
  if recursive  then recursiveM processDir dir  else do processDir dir; return ()
    where processDir dir = do
            dirContents  <-  getDirectoryContents_FileInfo ff dir
            process_f `unlessNull` (filter accept_f dirContents)   -- ���������� ��������������� �����, ���� �� ������ ������
            return                 (filter fiIsDir  dirContents)   -- ���������� ������ ������������ ��� ����������� ���������

{-# NOINLINE getDirectoryContents_FileInfo #-}
{-# NOINLINE findFiles_FileInfo #-}


----------------------------------------------------------------------------------------------------
---- ����� � ��������� ������, ��������������� �������� ��������� ----------------------------------
----------------------------------------------------------------------------------------------------

-- |������� ������ ������ �� �����
data FileFind = FileFind
    { ff_ep             :: !Int
    , ff_scan_subdirs   :: !Bool
    , ff_include_dirs   :: !(Maybe Bool)
    , ff_no_nst_filters :: !Bool
    , ff_filter_f       :: !(FileInfo -> Bool)
    , ff_group_f        :: !(Maybe (FileInfo -> FileGroup))
    , ff_arc_basedir    :: !String
    , ff_disk_basedir   :: !String
    }

-- |����� [����������] ��� �����, ��������������� ����� `filespec`, � ������� �� ������
find_files scan_subdirs filespec  =  find_and_filter_files [filespec] doNothing $
    FileFind { ff_ep             = -1
             , ff_scan_subdirs   = scan_subdirs
             , ff_include_dirs   = Just False
             , ff_no_nst_filters = True
             , ff_filter_f       = const True
             , ff_group_f        = Nothing
             , ff_arc_basedir    = ""
             , ff_disk_basedir   = ""
             }

-- |��������� ������ ���� ������ � ������������ � ��������
dir_list directory  =  find_and_filter_files [directory </> reANY_FILE] doNothing $
    FileFind { ff_ep             = 0
             , ff_scan_subdirs   = False
             , ff_include_dirs   = Just True
             , ff_no_nst_filters = True
             , ff_filter_f       = const True
             , ff_group_f        = Nothing
             , ff_arc_basedir    = ""
             , ff_disk_basedir   = ""
             }


-- |����� ��� �����, ��������������� �������� ������ `ff`,
-- � ������� �� ������
find_and_filter_files filespecs process_f ff = do
  concat ==<< withList (\list -> do  -- ���������������� ������ ������, ��������� � ������ �����������
    find_filter_and_process_files filespecs ff $ \files -> do
      process_f files
      list <<= files)

-- |����� ��� �����, ��������������� �������� ������ `ff`,
-- � ������� �� ������ �� ������ � �������� ����� ��������
find_and_filter_files_PROCESS filespecs ff pipe = do
  find_filter_and_process_files filespecs ff (sendP pipe)
  sendP pipe []  -- ������ "� ����-�� ��� ���������!" :)


-- |����� [����������] ��� �����, ����������� ������� `filespecs` � ��������� ������ `filter_f`,
-- � ��������� ��� ������ ������� ������, ��������� � ����� ��������, �������� `process_f`
find_filter_and_process_files filespecs ff@FileFind{ ff_ep=ep, ff_scan_subdirs=scan_subdirs, ff_include_dirs=include_dirs, ff_filter_f=filter_f, ff_group_f=group_f, ff_arc_basedir=arc_basedir, ff_disk_basedir=disk_basedir, ff_no_nst_filters=no_nst_filters} process_f

  -- ������������� ����� �� ����� ��������, � ���������� ������ �� ���� ����� ��������
  = do curdir  <-  getCurrentDirectory >>== translatePath
{-
       -- ����� ������ ��� � RAR
       let doit f = do
             let re = isRegExp f
             isdir <- isDirExists f
             if not re && isdir  then findRecursively f  else do
             if not re && -r-    then getStat f `catch` "WARNING: file %s not found"
             else                     find (re || !-r-) f
-}
       -- �������� ����� ��������� dir �� ��� ����� "dir dir/" ����� �������� ��� ������� � ��� ����� � ���
       modified_filespecs <- foreach filespecs $ \filespec -> do
         isDir <- if hasTrailingPathSeparator filespec
                    then return True
                    else dirExist (disk_basedir </> filespec)
         when isDir $ do
           find_files_in_one_dir curdir True [dropTrailingPathSeparator filespec]
         return$ (isDir &&& addTrailingPathSeparator) filespec
       --
       mapM_ (find_files_in_one_dir curdir False) $ sort_and_groupOn (filenameLower.takeDirectory) modified_filespecs

  where
    -- ���������� ������ �����, ����������� � ������ ��������
    find_files_in_one_dir curdir addDir filespecs = do
      findFiles_FileInfo root FindFiles{ff_process_f=process_f.map_group_f, ff_recursive=recursive, ff_disk_eq_filtered=disk_eq_filtered, ff_stored_eq_filtered=stored_eq_filtered, ff_parent_or_root=parent_or_root, ff_accept_f=accept_f}

      where dirname  =  takeDirectory (head filespecs)  -- ����� ��� ���� ����� �������
            masks    =  map takeFileName filespecs      -- ����� ��� ����� ����� ��������
            root     =  createParentDirFileInfo         -- ������� FileInfo ��� ����� ������:
                            dirname                     --   ������� ������� ��� ���������� ������
                            diskdir                     --   ������� ������� �� �����
                            arcdir                      --   ������� ������� � ������

            -- ������� ������� �� �����
            diskdir           =  disk_basedir </> dirname
            -- ����� ������ �� ����� � � ���. ������ ���������?
            disk_eq_filtered  =  diskdir==dirname
            -- ������ ���� � �������� �������� �� ����� ��� -ep2/-ep3
            full_dirname      =  curdir </> diskdir

            -- ������� ������� � ������
            arcdir  =  arc_basedir </> case ep of
               0 -> ""                        -- -ep:  exclude any paths from names
               1 -> ""                        -- -ep1: exclude base dir from names
               2 -> full_dirname.$stripRoot   -- -ep2: full absolute path without "d:\"
               3 -> full_dirname              -- -ep3: full absolute path with "d:\"
               _ -> dirname.$stripRoot        -- Default: full relative path
            -- �������� parent ��� root ������� � ����������� �� ����� -ep
            parent_or_root      =  if ep==0  then const root  else id
            -- ����� ������ ������ ������ � � ���. ������ ���������?
            stored_eq_filtered  =  arcdir==dirname && ep/=0

            -- ���� �� ��� ������� ��� "dir/"?
            dir_slash    =  dirname>"" && masks `contains` ""
            -- ����������� ����������� ���� ������� ����� "-r" ��� ���� �� ��� ������� ��� "dir/"
            recursive    =  scan_subdirs || dir_slash
            -- �������� � ������ ��� �����/��������, ���� ���� �� ��� ������� ��� "dir/" ��� "*" ��� "dir/*"
            include_all  =  dir_slash || masks `contains` reANY_FILE
            -- ��������, ������������ ����� ����� � �������� ����� �������� � ����������� ������:
            --   ��� ��������� ��� ������� �� ����� --[no]dirs, by default - ��� ������� "[dir/]* -r" || "dir/" � ���������� �������� ������ ������ -n/-s../-t..
            --   ��� ������ ����������� ������������ ��������� `filter_f` � ����� �� �����
            accept_f fi | fiIsDir fi  =  include_dirs `defaultVal` (addDir && baseName fi `elem` masks  ||  no_nst_filters && recursive && include_all)
                        | otherwise   =  filter_f fi && (include_all || match_filespecs fpBasename masks (fiFilteredName fi))
            -- ������������� � [FileInfo] ������ ����� fiGroup ��������, ���������� � group_f
            map_group_f = case group_f of
                            Nothing -> id
                            Just f  -> map (\x -> x {fiGroup = f x})

{-# NOINLINE find_files #-}
{-# NOINLINE find_and_filter_files #-}
{-# NOINLINE find_and_filter_files_PROCESS #-}
{-# NOINLINE find_filter_and_process_files #-}
