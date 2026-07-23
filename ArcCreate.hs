{-# OPTIONS_GHC -cpp -XNondecreasingIndentation -XScopedTypeVariables #-}
----------------------------------------------------------------------------------------------------
---- �������� � ��������� �������.                                                              ----
---- ����� �������������� ��� ������� �������� � ����������� �������:                           ----
----   create/a/f/m/u/ch/c/d/k/s/rr/j                                                           ----
---- ��������� runArchiveCreate ������ ������ ������, ������� ������ ������� � �������� �����, ----
----   ����� ��������� �������� �������� ��������� ��������� ������, ������ ������� ������,     ----
----   �������� � ������ ������ � �������� �����.                                               ----
---- ��� �������� ������� � ArcvProcessRead.hs � ArcvProcessCompress.hs                         ----
----------------------------------------------------------------------------------------------------
module ArcCreate where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Mem
import System.IO
#if defined(FREEARC_UNIX)
import System.Posix.Files hiding (fileExist)
#endif

import Utils
import Files
import Charsets            (i18n)
import Process
import Errors
import ByteStream
import FileInfo
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcExtract
import ArcvProcessRead
import ArcvProcessExtract
import ArcvProcessCompress


-- |���������� ������� ��������/��������� ������
runArchiveCreate pretestArchive
                 writeRecoveryBlocks
                                   command@Command {             -- ������ � ����������� �������:
      cmd_name            = cmd                  --   �������� �������
    , cmd_arcname         = arcname              --   �������� �����, ������� ������������ ����������
    , cmd_archive_filter  = archive_filter       --   �������� ������ �������������� ������ �� �������
    , cmd_added_arcnames  = find_added_arcnames  --   �������������� ������� ������
    , cmd_diskfiles       = find_diskfiles       --   �����, ������� ����� �������� � �����
    , opt_arccmt_str      = arccmt_str           --   ����� ����������� � ������, ���
    , opt_arccmt_file     = arccmt_file          --   ����, �� �������� �������� ����� ����������� � ������
    , opt_data_compressor = compressor           --   �������� ������
    } = do
  opt_testMalloc command &&& testMalloc  -- ���������� ����� ������
  -- ��� ������������� ������: find_files |> buffer 100_000 |> write_to_archive

  -- ������ sfx-����� ����� � ����������� EXE, ���� ������ �� �� ������ �������� ��� ������������ �����
  arcname <- do archiveExists <- fileExist arcname
                if cmd=="create" || not archiveExists
                  then return$ cmdChangeSfxExt command arcname
                  else return arcname
  command <- return command {cmd_arcname = arcname}

  -- ������� "create" ������ ������ ����� � ����
  when (cmd=="create")$  do ignoreErrors$ fileRemove arcname
  -- �������� ������������ � ������ ��������� ������ � ��������� ������ ���������, ���� ����������
  uiStartArchive command =<< limit_compression command compressor   -- ���������� ���������� ������� ������ � ��������� -lc
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  debugLog "Started"

  -- ��������� ��������� ���������� ��������� (������������) ������, ������� ��������.
  -- �����, ���� ����� ������� ��� �������� recovery info � ��������.
  -- ���� �� ������ ����� �����, �� ���������� ������ ������� "������".
  let abort_on_locked_archive archive footer = do
          when (ftLocked footer) $
              registerError$ GENERAL_ERROR ["0310 can't modify archive locked with -k"]
          pretestArchive command archive footer
  --
  uiStage "0249 Reading archive directory"
  updatingArchive <- fileExist arcname
  main_archive    <- if updatingArchive
                       then archiveReadInfo command "" "" archive_filter abort_on_locked_archive arcname
                       else return phantomArc
  debugLogList "There are %1 files in archive being updated" (arcDirectory main_archive)

  -- ����� �� ����� ����������� ������ (��� ������� "j") � ��������� �� ��������� ����������.
  -- �����, ���� ����� �� ���� ������� �������� recovery info � ��������.
  uiStartScanning
  added_arcnames <- find_added_arcnames
  debugLogList "Found %1 archives to add" added_arcnames
  added_archives  <- foreach added_arcnames (archiveReadInfo command "" "" archive_filter (pretestArchive command))
  debugLogList "There are %1 files in archives to add" (concatMap arcDirectory added_archives)
  let input_archives = main_archive:added_archives      -- ������ ���� ������� �������
      closeInputArchives = for input_archives arcClose  -- �������� �������� ���� ������� �������

  -- �������� ����������� � ������������ ������ ���� ���������� ������ ��� ������ �� ������������
  arcComment <- getArcComment arccmt_str arccmt_file input_archives (opt_parseFile command)

  -- ����� ����������� ����� �� ����� � ������������� �� ������
  uiStartScanning
  diskfiles <- find_diskfiles
  debugLogList "Found %1 files" diskfiles
  uiStage "0250 Sorting filelist"
  sorted_diskfiles <- (opt_reorder command &&& reorder) (sort_files command diskfiles)
  debugLogList "Sorted %1 files" sorted_diskfiles
  uiStartScanning  -- ������� ������� ��� ������ ������� ����������� ������

  -- �������� ������ ������, ������� ������ ������� � �������� �����, ���� �����������.
  -- ������ ������ �� ������������ ������, ������ ������ �� ����������� (�������� "j")
  -- � ���� �������, � ������ � �����. �������������� ��� ������ ���������� �� ����������.
  files_to_archive <- join_lists main_archive added_archives sorted_diskfiles command
  debugLogList "Joined filelists, %1 files" files_to_archive

  if null files_to_archive                    -- ���� �������� ����� �� �������� �� ������ �����
    then do registerWarning NOFILES           -- �� �������� �� ���� ������������
            closeInputArchives                --    ������� ������� ������
            ignoreErrors$ fileRemove arcname  --    ������� �����, ���� �� ����������� ����� ��������� (��������, � ������ ������� "arc d archive *")
            return (1,0,0,0)
    else do

  -- �������, ����������� �������������� (-d[f], -ac) ������ ���� ��� ������������ ���������� ������ �� ���� �� ������ warning'�
  postProcess_wrapper command $ \postProcess_processDir deleteFiles -> do

  -- ������ ��� �������� ����������� ������ ������� � ���������� ���������
  results <- ref (error "runArchiveCreate:results undefined")

  -- ��������� mtime ������ ��� ����� -tk
  old_arc_exist <- fileExist arcname
  arc_time <- if old_arc_exist  then getFileDateTime arcname  else return (error "runArchiveCreate:arc_time undefined")

  -- ��� ���������� ����� -tl �� ������ �������� ������ ���� ������������ � ����� ������ � ����� ����� ������ �� ���.
  --   ��� ����� � create_archive_structure_PROCESS ��������� ��������� `find_last_time`.
  --   �� �������� �� ������ ������ ������, ������������ � �����, � ��� ����������� ����� ������ �� ���.
  --   ���� ����� ����� ������������ ����� ����� ��������� ���������.
  last_time <- ref aMINIMAL_POSSIBLE_DATETIME
  let find_last_time dir  =  last_time .= (\time -> maximum$ time : map (fiTime.fwFileInfo) dir)
  let processDir dir      =  do when (opt_time_to_last command) (find_last_time dir)
                                postProcess_processDir dir  -- ������� ��������������� ���� ������ �������� ������ ������� ��������������� ������

  -- �������� ������������ � ������ �������� ������
  uiStartProcessing (map cfFileInfo files_to_archive) 0 0
  performGC   -- ��������� ����� ����� ���������� ��� ����� ������ ������ ��� ���������� ������ ������

  -- ������� �� ���������� ���������� ������������ ������ �� ��������� ���� � ���� �����, ��� ������ ��������� - ��������������� ���
  tempfile_wrapper arcname command deleteFiles pretestArchive $ \temp_arcname -> do
    ensureCtrlBreak "closeInputArchives" closeInputArchives $ do   -- ������� ������� ������ �� ���������� ���������
      bracketCtrlBreak "archiveClose:ArcCreate" (archiveCreateRW temp_arcname) (archiveClose) $ \archive -> do
        writeSFX (opt_sfx command) archive main_archive    -- ������ �������� ������ � ������ SFX-������
        -- �������� ������ - ������������������ ��������� ���������, ���������� ������ ���� �����:
        --   �������� ���������� ��������� ������ � ������ ������������� ������
        --   �������� �������� � ������ ������ ������ � �������� ����
        -- ����� ���� �������� ������� �������������� ����� (|>>>), ��� ��������� ������������ read-ahead ��������� ������
        let read_files          =  create_archive_structure_AND_read_files_PROCESS command archive main_archive files_to_archive processDir arcComment writeRecoveryBlocks results
            compress_AND_write  =  compress_AND_write_to_archive_PROCESS archive command
        backdoor <- newChan   -- ���� ����� ������������ ��� ����������� ���������� � ��������� ������ ������
        runP (read_files backdoor |>>> compress_AND_write backdoor)
      --debugLog "Archive written"

  when (opt_keep_time command && old_arc_exist) $ do   -- ���� ������������ ����� -tk � ��� ���� ���������� ������������� ������
    setFileDateTime arcname arc_time                   --   �� ������������ mtime ������
  when (opt_time_to_last command) $ do                 -- ���� ������������ ����� -tl
    setFileDateTime arcname =<< val last_time          --   �� ���������� �����&���� ����������� ������ �� �����&���� ����������� ������ ������� ����� � ���
  renameArchiveAsSFX arcname command                   -- ����������� �����, ���� � ���� ��� �������� ��� �� ���� ����� SFX-������
  val results                                          -- ��������� ���������� ���������� �������


----------------------------------------------------------------------------------------------------
---- ������������� ���������� ����� ��� �������� ������ --------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� � ������� ��� ����������� ��������� ������
temparc_prefix = "$$temparc$$"
temparc_suffix = ".tmp"

-- |��������� `action` � ������ ���������� ����� � ����� ������������� ���
tempfile_wrapper filename command deleteFiles pretestArchive action  =  find 1 >>= doit
  where -- ����� ��������� ��� ��� ���������� �����
        find n = do tempdir <- if opt_create_in_workdir command  then getTempDir  else return (takeDirectory filename)
                    createDirectoryHierarchy tempdir
                    let tempname = tempdir </> (temparc_prefix++show n++temparc_suffix)
                    found <- fileExist tempname
                    case found of
                        True  | n==999    -> registerError$ GENERAL_ERROR ["0311 can't create temporary file"]
                              | otherwise -> find (n+1)
                        False             -> return tempname

        -- ��������� ��������, ��������� ��������� ��� �����, �������������� � ����� ������������� ������������� �����
        doit tempname = do old_file <- fileExist filename      -- �� ��������� ���������� ������������� ������?
                           handleCtrlBreak "fileRemove tempname" (ignoreErrors$ fileRemove tempname) $ do
                             -- ��������� ���������
                             action tempname
                             -- ���� ������� ����� "-t", �� ������������ ������ ��� ��������� �����
                             when (opt_test command) $ do
                                 test_archive tempname (opt_keep_broken command)
                           handleCtrlBreak "Keeping temporary archive" (condPrintLineLn "n"$ "Keeping temporary archive "++tempname) $ do
                             -- ������� ��������������� �����, ���� ������������ ����� -d
                             deleteFiles
                             -- �������� ������ ����� �����
                             if old_file
                                 then fileRemove filename   -- ������ �� ���������, ��� ��� �� ��� ��� ����� ����
                                 else whenM (fileExist filename) $ do  -- ���� ���� � ������ ��������� ������ ������� �� ����� ���������, �� �������� �� ������
                                          registerError$ GENERAL_ERROR ["0312 output archive already exists, keeping temporary file %1", tempname]
                             fileRename tempname filename
                                 `catch` (\(_ :: SomeException) -> do
                                       condPrintLineLn "n"$ "Copying temporary archive "++tempname++" to "++filename
                                       fileCopy tempname filename; fileRemove tempname)
                           -- ���� ������� ����� "-t" � "--create-in-workdir", �� ��� ��� ������������ ������������� �����
                           when (opt_test command && opt_create_in_workdir command) $ do
                               test_archive filename (opt_keep_broken command || opt_delete_files command /= NO_DELETE)

        -- �������������� ����� � �����, ������ ���, ���� ��� ���� �������� ��������
        test_archive arcname keep_broken_archive = do
            w <- count_warnings $ do
                     testArchive command arcname pretestArchive
            -- ���������� ������ ������ ��� ���������� warning'��
            when (w/=0) $ do
                unless keep_broken_archive (ignoreErrors$ fileRemove arcname)
                registerError$ GENERAL_ERROR$ if keep_broken_archive
                                                 then ["0313 archive broken, keeping temporary file %1", arcname]
                                                 else ["0314 archive broken, deleting"]


----------------------------------------------------------------------------------------------------
---- ��������������, ����������� ������ ���� ��������� ������ ������� ------------------------------
----------------------------------------------------------------------------------------------------

-- |��������������, ����������� ������ ���� ��������� ������ �������:
--    ������� ������� ��������������� �����, ���� ������ ����� -d[f]
--    �������� � ��� �������� Archive, ���� ������ ����� -ac
postProcess_wrapper command archiving = do
  doFinally uiDoneArchive2 $ do
  case (opt_delete_files command/=NO_DELETE || opt_clear_archive_bit command) of
      False -> archiving (\dir->return()) (return())  -- ���� ����� ������� �� �����, �� ������ �������� archiving

      _ -> do files2delete <- ref []   -- ������ ������, ������� �� ������ �������
              dirs2delete  <- ref []   -- ������ ���������, ������� �� ������ �������
              let -- ���� ��������� �� ������ ��������� ������ ������� ��������������� ������ � ���������,
                  -- � ��� ���������� �� ��� � ���, ����� ����� ��������� ��������� ��������� ������� ��
                  processDir filelist0  =  do
                      let filelist = map fwFileInfo$ filter isFILE_ON_DISK filelist0
                          (dirs,files)  =  partition fiIsDir filelist
                      evalList files  `seq`  (files2delete ++= files)
                      evalList dirs   `seq`  (dirs2delete  ++= dirs )
                  -- ������� ��������������� ����� � ��������
                  deleteFiles = when (opt_delete_files command /= NO_DELETE) $ do
                                    -- ������� ��������, ��� ������������� ��������� �������� � �����
                                    let superRemove removeAction fi = do
                                            let filename = diskName fi
                                            removeAction filename `catch` \(e :: SomeException) -> do
                                                -- Remove readonly/hidden/system attributes and try to remove file/directory again
                                                ignoreErrors$ clearFileAttributes filename
                                                ignoreErrors$ removeAction filename
                                    -- �������� ������
                                    condPrintLineLn "n"$ "Deleting successfully archived files"
                                    files <- val files2delete
                                    for files $ \fi -> do
                                        whenM (check_that_file_was_not_changed fi) $ do
                                            superRemove fileRemove fi
                                    -- �������� ���������
                                    when (opt_delete_files command == DEL_FILES_AND_DIRS) $ do
                                        dirs <- val dirs2delete
                                        for (reverse dirs) (superRemove dirRemove)   -- �������� ������ ����������� � ������� ������, �� ���� ������������ ������� � ������ ������ ��������. ��� ��� reverse ��������� ������� ������� �������� ��������

              -- ��������� ���������, ������ ������� ��������������� ����� � ������ files2delete � dirs2delete.
              -- ������� ����� ����� ���������, ���� ������ ����� -d[f]
              result <- archiving processDir deleteFiles
              -- �������� ������� "������������" � ������� ����������� ������, ���� ������ ����� -ac
              when (opt_clear_archive_bit command) $ do
                  condPrintLineLn "n"$ "Clearing Archive attribute of successfully archived files"
                  files <- val files2delete
                  for files $ \fi -> do
                      whenM (check_that_file_was_not_changed fi) $ do
                          clearArchiveBit.fpFullname.fiDiskName$ fi
              return result

-- |���������, ��� ���� �� ��������� � ������� ���������
check_that_file_was_not_changed fi = do
    fileWithStatus "check_that_file_was_not_changed" (fpFullname.fiDiskName$ fi) $ \p_stat -> do
        size <- stat_size  p_stat
        time <- stat_mtime p_stat
        return (size==fiSize fi  &&  time==fiTime fi)


----------------------------------------------------------------------------------------------------
---- ��������������� �������� ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ����������� ��������� ������ �� �����, ���������� ������ -z,
-- ��� ������������� ������������ ������� �������, � ������� ��� �� �����
getArcComment arccmt_str arccmt_file input_archives parseFile = do
  -- ���������� �����������, �������� � ��������� ������, ���� ����
  if arccmt_str>""  then do uiPrintArcComment arccmt_str
                            return arccmt_str
    else do
  let old_comment = joinWith "\n\n" $ deleteIf null $ map arcComment input_archives
  -- � ����������� �� �������� ����� "-z":
  case arccmt_file of
  -- ������ ����������� � stdin
    ""   -> uiInputArcComment old_comment
  -- ������� ������ �����������
    "-"  -> return ""
  -- ����������� ������������ ����������� (�� ���������):
    "--" -> do uiPrintArcComment old_comment
               return old_comment
  -- ��������� ����� ����������� �� ���������� �����:
    _    -> do newcmt <- parseFile 'c' arccmt_file >>== joinWith "\n"
               uiPrintArcComment newcmt
               return newcmt

-- |�������� SFX-������ � ������ ������������ ������
writeSFX sfxname archive old_archive = do
  let oldArchive = arcArchive old_archive
      oldSFXSize = ftSFXSize (arcFooter old_archive)
  case sfxname of                                      -- � ����������� �� �������� ����� "-sfx":
    "-"      -> return ()                              --   ������� ������ sfx-������
    "--"     -> unless (arcPhantom old_archive) $ do   --   ����������� sfx �� ��������� ������ (�� ���������)
                  archiveCopyData oldArchive 0 oldSFXSize archive
    filename -> bracket (archiveOpen sfxname              --   ��������� ������ sfx �� ���������� �����
                          `catch` (\(_ :: SomeException) -> registerError$ GENERAL_ERROR ["0315 can't open SFX module %1", sfxname]))
                        (archiveClose)
                        (\sfxfile -> do size <- archiveGetSize sfxfile
                                        archiveCopyData sfxfile 0 size archive)

-- |����� ��� ������ � ������������ � ���, ��� �� �������� ��� �������� ������ �� ���� SFX-������
cmdChangeSfxExt command  =  changeSfxExt (opt_noarcext command) (opt_sfx command)

changeSfxExt opt_noarcext opt_sfx arcname =
  case (opt_noarcext, opt_sfx) of
--  ���������, ��������� ������ �������������� � SFX ������ ������� GUI
--  (True, _)     -> arcname                -- �� ������ ����������, ���� ������� ����� --noarcext
    (_   , "--")  -> arcname                --   ��� �� ������� ����� "-sfx"
                                            -- ��� "-sfx-" ���������� �������� �� ".arc"
    (_   , "-")   -> if takeExtension arcname == aDEFAULT_SFX_EXTENSION
                       then replaceExtension arcname aDEFAULT_ARC_EXTENSION
                       else arcname
                                            -- ��� "-sfx..." ���������� �������� �� ".exe"
    _             -> if takeExtension arcname == aDEFAULT_ARC_EXTENSION
                       then replaceExtension arcname aDEFAULT_SFX_EXTENSION
                       else arcname

-- |������������� ����� � ������������ � ��� SFX-������
renameArchiveAsSFX arcname command = do
  let newname = cmdChangeSfxExt command arcname
  when (newname/=arcname) $ do
    condPrintLineLn "n"$ "Renaming "++arcname++" to "++newname
    fileRename arcname newname
#if defined(FREEARC_UNIX)
  -- �������� ��� ������ "+x" �� ��������� �����, ���� ��� sfx-������� ���������
  when (opt_sfx command /= "--") $ do
    let isSFX   = opt_sfx command /= "-"
    oldmode    <- fmap fileMode (fileGetStatus newname)
    let newmode = foldl (iif isSFX unionFileModes removeFileModes) oldmode executeModes
    fileSetMode newname newmode
#endif

-- |�������������� ������ ��� ��������� �����, ����������� � ����� �� ����� `temp_arcname`
testArchive command temp_arcname pretestArchive = do
  let test_command = command{ cmd_name           = "t"           -- ���������
                            , cmd_arcname        = temp_arcname  -- � ��������� ������
                            , opt_arc_basedir    = ""            -- ��� �����
                            , opt_disk_basedir   = ""            -- ...
                            , cmd_archive_filter = const True    -- ...
                            , cmd_subcommand     = True          -- ��� ���������� (������������ ������ ��������)
                            , opt_pretest        = 1             -- �� ����� ��������� ������������ ����� �������������, �� recovery info ��������� ���� :)
                            }
  uiStartSubCommand command test_command
  results <- runArchiveExtract pretestArchive test_command
  uiDoneSubCommand command test_command [results]

