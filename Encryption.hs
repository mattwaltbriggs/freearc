----------------------------------------------------------------------------------------------------
---- ����������, ������������ � ����������������� PRNG.                                         ----
---- ��������� generateEncryption ��������� � ������� ���������� ������ ��������(�) ����������. ----
---- ��������� generateDecryption ��������� ����� � ������ ��������� ������+����������,         ----
---- ��������� generateRandomBytes ���������� ������������������ �����. ��������� ����          ----
----------------------------------------------------------------------------------------------------
module Encryption (generateEncryption, generateDecryption, generateRandomBytes) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Data.Char
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe

import EncryptionLib
import Utils
import Errors
import Compression

---------------------------------------------------------------------------------------------------
---- ���������� � ������������ --------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |���������� ��� �������, ����������� � ������ ������ �������� ����������:
-- ������ �������� ���� ���������� (��� ��������� �������������)
-- ������ ������ ��������������� ������ (��� ���������� � ������)
generateEncryption encryption password = do
    addRandomness
    result <- foreach (split_compressor encryption) $ \algorithm -> do
        initVector <- generateRandomBytes (encryptionGet "ivSize"  algorithm)
        salt       <- generateRandomBytes (encryptionGet "keySize" algorithm)
        let numIterations = encryptionGet "numIterations" algorithm
            checkCodeSize = 2
            (key, checkCode) = deriveKey algorithm password salt numIterations checkCodeSize
        return (algorithm++":k"++encode16 key++":i"++encode16 initVector
               ,algorithm++":s"++encode16 salt++":c"++encode16 checkCode
                         ++":i"++encode16 initVector)
    return ((++map fst result), (++map snd result))


-- |���������� compressor, ����������� �� ������, ������� � ���� ����������,
-- ����������� ��� �����������
generateDecryption compressor decryption_info  =  mapM addKey compressor   where
  -- ���������� �������� ����������, ������� � ���� key, ����������
  -- �� ��������� ������������� ������ � salt, ������������ � ���������� ���������
  addKey algorithm | not (isEncryption algorithm)
                               = return (Just algorithm)    -- non-encryption algorithm stays untouched
                   | otherwise = do
    -- ������� �� ������ ��������� ���������, ����������� ��� ���������� � �������� �����
    let name:params   = split_method algorithm
        param c       = params .$map (splitAt 1) .$lookup c   -- ����� �������� ��������� �
        salt          = param "s" `defaultVal` (error$ algorithm++" doesn't include salt") .$decode16
        checkCode     = param "c" `defaultVal` "" .$decode16
        numIterations = param "n" `defaultVal` (error$ algorithm++" doesn't include numIterations") .$readInt

    -- ������������� ������� ������� ����������� �, ���� �������,
    -- ������� � ���� ����� ������, �������� �������������
    let (dont_ask_passwords, mvar_passwords, keyfiles, ask_decryption_password, bad_decryption_password) = decryption_info
    modifyMVar mvar_passwords $ \passwords -> do
      passwords_list <- ref passwords

      -- ����������� ����������� �� ����� ���������� keyfiles � ����� ��� ���
      let checkPwd password  =  firstJust$ map doCheck (keyfiles++[""])
            where -- ���� ����������� �� checkCode �������, �� ���������� ��������� ���� ��������� ����������, ����� - Nothing
                  doCheck keyfile  =  recheckCode==checkCode  &&&  Just key
                    -- �������� ���� ����������� key � recheckCode, ������������ ��� ������� �������� ������������ ������
                    where (key, recheckCode) = deriveKey algorithm (password++keyfile) salt numIterations (length checkCode)

      -- ��������� ������� ������ ��� ����������� �����.
      -- ������ ��������� ������ ����������� � ������� checkPwd.
      -- ���� �� ���� �� ��� ��������� ������� �� �������, �� �� ����������� � ������������ �����
      let findDecryptionKey (password:pwds) = do
            case (checkPwd password) of            -- ���� ����������� � checkPwd ������ �������
              Just key -> return (Just key)        --   �� ���������� ��������� ���� ��������� ����������
              Nothing  -> findDecryptionKey pwds   --   ����� - ������� ��������� ������

          findDecryptionKey [] = do   -- ���� �� �������� ���� �� ���� ������ ������ �� �������
            -- ���� ������������ ����� -p-/-op- ��� ������������ ����� ������ ������ -
            -- ������, ���� ���� ������������ ��� �� �������
            if dont_ask_passwords  then return Nothing   else do
              password <- ask_decryption_password
              if password==""        then return Nothing   else do
                -- ������� ����� ������ � ������ �������, ����������� ��� ����������
                passwords_list .= (password:)
                case (checkPwd password) of               -- ���� ����������� � checkPwd ������ �������
                  Just key -> return (Just key)           --   �� ���������� ��������� ���� ��������� ����������
                  Nothing  -> do bad_decryption_password  --   ����� - ����������� ������ ������
                                 findDecryptionKey []
      --
      key <- findDecryptionKey passwords
      pl  <- val passwords_list
      return (pl, key.$fmap (\key -> algorithm++":k"++encode16 key))


-- |������� �� password+salt ���� ���������� � ��� ��������
deriveKey algorithm password salt numIterations checkCodeSize =
    splitAt keySize $ pbkdf2Hmac password salt numIterations (keySize+checkCodeSize)
    where   keySize = encryptionGet "keySize" algorithm


-- |Check action result and abort on (internal) error
check test msg action = do
  res <- action
  unless (test res) (fail$ "Error in "++msg)

-- |OK return code for LibTomCrypt library
aCRYPT_OK = 0


---------------------------------------------------------------------------------------------------
---- ����������������� ��������� ��������� ������������������� ���� -------------------------------
---------------------------------------------------------------------------------------------------

-- |�������� ��������� ����������, ���������� �� ��, � PRNG
addRandomness = withMVar prng_state addRandomnessTo
addRandomnessTo prng = do
  let size = 4096
  allocaBytes size $ \buf -> do
    bytes <- systemRandomData buf (i size)
    check (==aCRYPT_OK) "prng_add_entropy" $
      prng_add_entropy buf (i bytes) prng

-- |��������� ��������� ������������������ ���� ��������� �����
generateRandomBytes bytes = do
  withMVar prng_state $ \prng -> do
    allocaBytes bytes $ \buf -> do
      check (==i bytes) "prng_read" $
        prng_read buf (i bytes) prng
      peekCAStringLen (buf, bytes)

-- |����������, �������� ��������� PRNG
{-# NOINLINE prng_state #-}
prng_state :: MVar (Ptr CChar)
prng_state = unsafePerformIO $ do
   prng <- mallocBytes (i prng_size)
   prng_start prng
   addRandomnessTo prng
   newMVar prng

-- |Fill buffer with system-generated pseudo-random data
foreign import ccall unsafe "Environment.h systemRandomData"
  systemRandomData :: Ptr CChar -> CInt -> IO CInt

