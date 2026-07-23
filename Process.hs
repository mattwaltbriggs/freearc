---------------------------------------------------------------------------------------------------
---- "����������������� ���������������� ��������", ��� ������� � ����� �����.                 ----
---------------------------------------------------------------------------------------------------
-- |
-- Module      :  Process
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Process where
{-
�������� ����������� � ������� ����������� "|>" ��� "|>>>" � ����������� �� ���������� �������� runP:
    runP( read_files |>>> compress |> write_data )
�������� ����������� ����������� ��������� ����, ��� ��� �� ������� ������������ ������� forkOS.
������ ������� ����������� ������� ��������, ������� �������� �������������� �������� ���� Pipe.
  � ���� ���������� ����� ��������� �������� receiveP ��� ��������� ������ �� �����������
  �������� � ������, � �������� sendP ��� ������� ������ ���������� �������� � ������:
    compress pipe = foreverM (do data <- receiveP pipe; .....; sendP pipe compressed_data)
������ �� �������� � �������� ���������� "����� �������". � ����������� �� ��������������
  ��� �������� ����� ����� ���������� �������� - "|>" ��� "|>>>" - � ����� ����� ����� ����������
  ����� ��������� ������ ���� ��� �������������� ���-�� �������� (����������� � ������� MVar/Chan,
  ��������������).
������ ����� ����� �������� � �������� ������� ("������ ������") ���������� send_backP � receive_backP.
  ����� �������� ����� ������ ����� �������������� �������. ��� ����� ������������, ��������,
  ��� ������������� ���������� ��������, �������������, ����������� �������������� ��������
  (��������, ������� �����/������):
    �������������: sendP pipe (buf,len); receive_backP pipe; ������ ����� ��������
    �����������:   (buf,len) <- receiveP pipe; hPutBuf file buf len; send_backP pipe ()
�������� runP ����������� ���������, ��� ����������� �� ��������� ���������� ���������� ��������
  � ������� (���� ���� ��������� �������� ��� �� �����������). ���� ������ ������� � ������
  ����������� �������� ������������ � ���������� (�.�. ��������� �������� receiveP/send_backP) ���
  ��������� ������� �������� ������������ �� ��������� - �� ��������������� ������.
�������� runAsyncP ��������� ������� ��� ������� ��������� ���������� � ���������� Pipe ��� ������
  � ���(�). � ���� ������ � ������ ������� � ������� ����� �������� � "����������", � ��������� - ��
  "���������", ���� ��� � �� �����������:
    pipe <- runAsyncP compress; sendP pipe data; compressed_data <- receiveP pipe
    pipe <- runAsyncP( compress |> write_data ); sendP pipe data
    pipe <- runAsyncP( read_files |>>> compress ); compressed_data <- receiveP pipe
    runAsyncP( read_files |>>> compress |> write_data )
  ������� � �������� ������� ���������� ����������� �������� - (����) ��������������.
-}

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

-- |�������� ���������� ���� ���������������� ���������:
-- �������� ����� ������� ���������� ������� ������� �������.
-- "|>" ������ �������������� �������, � "|>>>" - ������� �������������� �����
infixl 1  |>, |>>>

p1 |>   p2 = createP p1 p2 newEmptyMVar
p1 |>>> p2 = createP p1 p2 newChan

createP p1 p2 create_inner (Pipe pid finished income income_back outcome outcome_back) = do
  inner       <- create_inner      -- ����� ����� p1 � p2 (MVar ��� Chan)
  inner_back  <- newChan           -- �������� ����� ����� p1 � p2
  p1_finished <- newEmptyMVar      -- ������� ���������� ���������� p1

  -- �������� ������ ������� � ��������� �����, � ������ �������� ��������
  p1_id <- forkIO$ (p1 (Pipe pid finished income income_back inner inner_back) >> return ())
                       `finally` (putMVar p1_finished ())
  --
  p2 (Pipe (Just p1_id) (Just p1_finished) inner inner_back outcome outcome_back)
  takeMVar p1_finished
  return ()


-- |��������� ��������������� �������, ��������� ���������� "|>" � "|>>>"
runP p = do
  p (Pipe Nothing
          Nothing
          (error "First process in runP tried to receive")
          (error "First process in runP tried to send_back")
          (error "Last process in runP tried to send")
          (error "Last process in runP tried to receive_back"))

-- |��������� ������� ���������� � ���������� ����� ��� ������ � ���
runAsyncP p = do
  income  <- newEmptyMVar
  outcome <- newEmptyMVar
  income_back  <- newChan
  outcome_back <- newChan
  parent_id    <- myThreadId
  p_finished   <- newEmptyMVar
  p_id         <- forkIO (p (Pipe Nothing Nothing income income_back outcome outcome_back)
--                            `catch` (\e -> do killThread parent_id; throwIO e)
                            `finally` putMVar p_finished ())
  return (Pipe (Just p_id) (Just p_finished) outcome outcome_back income income_back)


-- |����� ������ � ��������� ����������, ������� �������� � ��� ������������ ������ �������.
-- ����� ����� 6 ��������� - �� ����������� (����������� ����������) ��������,
--                           MVar-����������, ��������������� � ��� ����������,
--                           ������� ������, ������� �������������,
--                           �������� ������, ��������� �������������
data Pipe a b c d  =  Pipe (Maybe ThreadId) (Maybe (MVar ())) a b c d
killP    pipe@(Pipe (Just pid) _ _ _ _ _)                                  = killThread pid >> joinP pipe
joinP         (Pipe _ (Just finished) _ _ _ _)                             = takeMVar finished
receiveP      (Pipe pid finished income income_back outcome outcome_back)  = getP income
sendP         (Pipe pid finished income income_back outcome outcome_back)  = putP outcome
receive_backP (Pipe pid finished income income_back outcome outcome_back)  = getP outcome_back
send_backP    (Pipe pid finished income income_back outcome outcome_back)  = putP income_back

-- |�������� �������� �������� - "�����������" ��������� ������ ���� - ���, ��� ���� �� ��� ������
-- ����������� ������� � �������. �� ��� ����� ��� �������� ���������� ���� ��������, ������������
-- ���������
send_back_itselfP (Pipe pid finished income income_back outcome outcome_back)  =  putP outcome_back


-- |������� ������ ����� ���������� - ����� ����� ��� ��� MVar, ��� � Chan
class PipeElement e where
  getP :: e a -> IO a
  putP :: e a -> a -> IO ()

instance PipeElement MVar where
  getP = takeMVar
  putP = putMVar

instance PipeElement Chan where
  getP = readChan
  putP = writeChan

-- |������-����� �������� - ������� �� ���� ���� �������� ������� ��� ��������� � ������� ������
data PairFunc a = PairFunc (IO a) (a -> IO ())

instance PipeElement PairFunc where
  getP (PairFunc get_f put_f) = get_f
  putP (PairFunc get_f put_f) = put_f

-- |��������� ������� �������� � 4 ��������� ��� �������� ������� �����/������
runFuncP p receive_f send_back_f send_f receive_back_f  =
  p (Pipe Nothing
          Nothing
          (PairFunc receive_f      undefined)
          (PairFunc undefined      send_back_f)
          (PairFunc undefined      send_f)
          (PairFunc receive_back_f undefined))

{-# NOINLINE createP #-}
{-# NOINLINE runP #-}
{-# NOINLINE runAsyncP #-}
{-# NOINLINE runFuncP #-}


-- ������ �������������:
{-
exampleP = do
  -- Demonstrates using of "runP"
  print "runP: before"
  runP( producer 5 |> transformer (++"*2") |> transformer (++"+1") |> printer "runP" )
  print "runP: after"

  -- Demonstrates using of "runAsyncP" to run computation as parallelly computed function
  pipe <- runAsyncP (transformer (++" modified"))
  sendP pipe "value"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" with "|>"
  pipe <- runAsyncP( transformer (++"*2") |> transformer (++"+1") )
  sendP pipe "7"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" to run asynchronous process
  print "runAsyncP: before"
  pipe <- runAsyncP( producer 7 |> printer "runAsyncP" )
  print "runAsyncP: after?"

producer n pipe = do
  mapM_ (sendP pipe.show) [1..n]
  sendP pipe "0"

transformer f pipe = do
  n <- receiveP pipe
  sendP pipe (f n)
  transformer f pipe

printer str pipe = do
  n <- receiveP pipe
  when (head n/='0')$  do print$ str ++ ": " ++ n
                          printer str pipe
-}

{- Design principles:
1. �������� � runP ������ ����������� ����� �������. ��� ��������� ������
�������������� ������ ��� ������� � ����, ��� ������ ������� �
����������� ��������� ��� ����������� ������ � ���������� ������, ���
������ � ����������� �������� ������ ����� ��������
2. runP ������ ��������� ��� �������� � �������������� ������ � ����������
�� ����������. ����� �� runP ���������� ����������� ������ �����
���������� ������ ���� ���������
3. ��� ���������� �������� ���������� ������� � ����������� ������ ��������
���������� ��� ����, ����� ����������� ��� ����� ������� (����� ����
�������������� ��� ������� ������ �������� ���������� � ���� �������).
��������� �� ������� ������ �������� ������ ���������� � ����������
������� ������ ��� ������� �� ��������� (tryReceiveP, eofP)
4. ��� ������������� ��������������� ���������� � ����� �� ��������� ���
��������� �������� � ����������� ������ ���� ���������� (�������� �������
KillThread) � ��� ���������� �������������� � �������� ��������
5. runP (p1 |> p2 |> protectP p) �������� ������� `p` �� ����������� � ��� ����������,
������ ����� ����������� �������� ������ ��������������� � ��������� ������
6. ��� �������� ���������� ��������, ����������� �� runAsyncP ���
����������� �������� � ����������� ������ �������� joinP pipe
7. "p |> yP p1 p2" �������� ����� ������ �������� ���� ������
8. killP pipe ������� ��� �������� �� ����������� ���������� �����������
9. ����� ������� � ����������� �������� ��� �������� ���������, �������
��������� ������� �/��� �������� ������� (������������ getP/putP?)
10. new_pipe <- insertOnInputP old_pipe process - �������� ����� ������� ����� ����� ������
    new_pipe <- insertOnOutputP old_pipe process - �������� ����� ������� ����� ������ ������

p1 |> p2   -->  PChain p1 p2 ?

(p1 |> p2) pipe{ MainThreadId, ref_threads... }
  p2_threadId <- forkIO $ (p2 pipe2  >> writeIORef pipe.isEof True - �� ��������� ������� ��������)
                          `catch` (throwTo MainThreadId)
  addToMVar ref_threads p2_threadId
  p1 pipe1

p1 |> (p2 |> p3)
  forkIO (forkIO p3; p2)
  p1

runP p =
  p_threadId <- forkIO$ p pipe{ MainThreadId = MyThreadId, ref_threads = newIORef [], ...}
  addToMVar ref_threads p_threadId
  wait them all `catch` (\e -> mapM killThread ref_threads; throw e)

11. ������ ����������� ����� - ����, ������� ������� ����, ���������� ��� ���������� � ��������
    ���������� ���������� ����
-}


{-New design guidelines:
1. a|>b ����������� ��� "fork a; b"
2. ��� ���������� b ��������� ���������� a
3. ��� ������������� � ����� �� ��������� ��������������� �������
   ���� ������� ��� �������� � ����������� � �������������� ���� ������ � �������� ���������
-}
