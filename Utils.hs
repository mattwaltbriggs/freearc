{-# OPTIONS_GHC -cpp -XOverlappingInstances -XUndecidableInstances -XNoMonomorphismRestriction -XFunctionalDependencies -XFlexibleInstances #-}
---------------------------------------------------------------------------------------------------
---- ��������������� �������: ������ �� ��������, ��������, ����������� �����������,           ----
----   ��������� ������. ��������� ����������� � IORef-�����������,                            ----
----   ����������� ������� �������� � ����������� �������� ���������.                          ----
---------------------------------------------------------------------------------------------------
module Utils (module Utils, module CompressionLib) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Either
import Data.IORef
import Data.List hiding (sortOn)
import Data.Maybe
import Data.Word
import Debug.Trace
import Foreign.Marshal.Utils
import Foreign.Ptr

import CompressionLib (MemSize,b,kb,mb,gb,tb)

---------------------------------------------------------------------------------------------------
---- �������� define's ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

#if !defined(FREEARC_WIN) && !defined(FREEARC_UNIX)
#error "You must define OS!"
#endif

#if !defined(FREEARC_INTEL_BYTE_ORDER) && !defined(FREEARC_MOTOROLA_BYTE_ORDER)
#error "You must define byte order!"
#endif


---------------------------------------------------------------------------------------------------
---- ������ :) ------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |������� 4-�������� ����� �� ��������� ������
#if defined(FREEARC_INTEL_BYTE_ORDER)
make4byte b0 b1 b2 b3 = b0+256*(b1+256*(b2+256*b3)) :: Word32
#else
make4byte b0 b1 b2 b3 = b3+256*(b2+256*(b1+256*b0)) :: Word32
#endif

-- |��������� �����, ����������� �� ������ ������. ���������� ����� ������������ ���������,
-- ����������� ����� ���� (b/k/f/...), ���� �� ��� - ������������ `default_specifier`.
-- ��������� ������������ � ���� ����, ������ ������� � ������� - `b`, ���� ��������� ������� � ������,
-- ��� ������ ������, ���������� ����� �����, ��� ���������� ��� `default_specifier`.
parseNumber num default_specifier =
  case (span isDigit$ strLower$ num++[default_specifier]) of
    (digits, 'b':_)  ->  (readI digits     , 'b')
    (digits, 'k':_)  ->  (readI digits * kb, 'b')
    (digits, 'm':_)  ->  (readI digits * mb, 'b')
    (digits, 'g':_)  ->  (readI digits * gb, 'b')
    (digits, 't':_)  ->  (readI digits * tb, 'b')
    (digits, '^':_)  ->  (2 ^ readI digits , 'b')
    (digits,  c :_)  ->  (readI digits     ,  c )

-- |������������ ������ �������, �� ��������� - � ������
parseSize memstr =
    case (parseNumber memstr 'b') of
        (bytes, 'b')  ->  bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |������������ ������ ������ ������: "512b", "32k", "8m" � ��� �����. "24" �������� 24mb
parseMem memstr =
    case (parseNumber memstr 'm') of
        (bytes, 'b')  ->  clipToMaxMemSize bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |���������� parseMem, �� � ����������� ��������� ������ � ���� 75%/75p (�� ������ ������ ������)
parseMemWithPercents memory memstr =
    case (parseNumber memstr 'm') of
        (bytes,    'b')  ->  clipToMaxMemSize$ bytes
        (percents, c) | c `elem` "%p"
                         ->  clipToMaxMemSize$ (memory * percents) `div` 100
        _                ->  error$ memstr++" - unrecognized size specifier"

-- ������ ������� � ������������� ������������ � MemSize ����� ��� ����� ������ �������� ������?
clipToMaxMemSize x | x < i(maxBound::MemSize) = i x
                   | otherwise                = i(maxBound::MemSize)

readI = foldl f 0
  where f m c | isDigit c  =  fromIntegral (ord c - ord '0') + (m * 10)
              | otherwise  =  error ("Non-digit "++[c]++" in readI")

readInt :: String -> Int
readInt = readI

readSignedInt ('-':xs) = - readInt xs
readSignedInt      xs  =   readInt xs

isSignedInt = all isDigit.tryToSkip "-"

lb :: Integral a =>  a -> Int
lb 0 = 0
lb 1 = 0
lb n = 1 + lb (n `div` 2)


{-# NOINLINE parseNumber          #-}
{-# NOINLINE parseSize            #-}
{-# NOINLINE parseMem             #-}
{-# NOINLINE parseMemWithPercents #-}
{-# NOINLINE readI                #-}
{-# NOINLINE readInt              #-}


-- |��������� �������� ��� ����� ������� ������ ��������
infixl 9  .$
infixl 1  >>==, ==<<, =<<., .>>=, .>>, .>>==, ==<<.
a.$b         =  b a                -- ������� $ � ����������� �������� ����������
a>>==b       =  a >>= return.b     -- ������� >>=, � ������� ������ �������� �������c� � ��������
a==<<b       =  return.a =<< b     -- ������� =<<, � ������� ������ �������� �������c� � ��������
(a=<<.b) c   =  a =<< b c          -- ������� =<< ��� ���������� � mapM � ���� �������� ������
(a.>>=b) c   =  a c >>= b          -- ������� >>= ��� ���������� � mapM � ���� �������� ������
(a.>>b)  c   =  a c >> b           -- ������� >> ��� ���������� � mapM � ���� �������� ������
(a==<<.b) c  =  return.a =<< b c   -- ������� ==<< ��� ���������� � mapM � ���� �������� ������
(a.>>==b) c  =  a c >>= return.b   -- ������� >>== ��� ���������� � mapM � ���� �������� ������

-- ���� ������, ������� �������� �� ���������
class    Defaults a      where defaultValue :: a
instance Defaults ()     where defaultValue = ()
instance Defaults Bool   where defaultValue = False
instance Defaults [a]    where defaultValue = []
instance Defaults (a->a)               where defaultValue = id
instance Defaults (Maybe a)            where defaultValue = Nothing
instance Defaults (a->IO a)            where defaultValue = return
instance Defaults a => Defaults (IO a) where defaultValue = return defaultValue
instance Defaults Int                  where defaultValue = 0
instance Defaults Integer              where defaultValue = 0
instance Defaults Double               where defaultValue = 0

class    TestDefaultValue a       where isDefaultValue :: a -> Bool
instance TestDefaultValue Bool    where isDefaultValue = not
instance TestDefaultValue [a]     where isDefaultValue = null
instance TestDefaultValue Int     where isDefaultValue = (==0)
instance TestDefaultValue Integer where isDefaultValue = (==0)
instance TestDefaultValue Double  where isDefaultValue = (==0)

infixr 3  &&&
infixr 2  |||

-- |���� �������� �������� �� ���������
a ||| b | isDefaultValue a = b
        | otherwise        = a

-- |���������� ������ ��������, ���� ������ �� �������� ��������� �� ���������
a &&& b | isDefaultValue a = defaultValue
        | otherwise        = b

-- |��������� b ���� a (�������� ���������� � �������� � ����� ������)
infixr 0 `on`
on b a | isDefaultValue a = return ()
       | otherwise        = b >> return ()

-- |��������� ������� f � ������ ������ ���� �� �� ������
unlessNull f xs  =  xs &&& f xs

-- |������������ ������� concatMap
concatMapM :: Monad io => (a -> io [b]) -> [a] -> io [b]
concatMapM f x  =  mapM f x  >>==  concat

-- |�������� ����������
whenM cond action = do
  allow <- cond
  when allow
    action

unlessM = whenM . liftM not

-- |��������� `action` ��� ���������, ������������ `x`, ���� ��� �� Nothing
whenJustM  x action  =  x >>= (`whenJust` action)

whenJustM_ x action  =  x >>= (`whenJust_` action)

whenJust   x action  =  x .$ maybe (return Nothing) (action .>>== Just)

whenJust_  x action  =  x .$ maybe (return ()) (action .>> return ())

-- |��������� `action` ��� ���������, ������������ `x`, ���� ��� "Right _"
whenRightM_ x action  =  x >>= either doNothing (action .>> return ())

-- |��������� onLeft/onRight ��� ���������, ������������ `x`
eitherM_ x onLeft onRight  =  x >>= either (onLeft  .>> return ())
                                           (onRight .>> return ())

-- |��������� ��� ������� �������� �� ������ � ���������� ��������� ��� ������
foreach = flip mapM

-- |��������� ��� ������� �������� �� ������
for = flip mapM_

-- |������� ������ �������� ������� ��, ��� ������ ����������� ���� ��������� � ����� :)
doFinally = flip finally

-- |��������� onError ��� ������ � acquire, � action � ��������� ������
handleErrors onError acquire action = do
  x <- try acquire
  case x of
    Left  (err :: SomeException) -> onError
    Right res -> action res

-- |�������� � ������ ��, ��� ����� ��������� � �����
atExit a b = (b>>a)

-- |��������� �������� ������ ���� ���, ��� var=True
once var action = do whenM (val var) action; var =: False
init_once       = ref True

-- |�������� �� ����� ������, ������� �� ������ ������ ���������
doNothing0       :: IO ()
doNothing0       = return ()
doNothing        :: a -> IO ()
doNothing  _     = return ()
doNothing2       :: a -> b -> IO ()
doNothing2 _ _   = return ()
doNothing3       :: a -> b -> c -> IO ()
doNothing3 _ _ _ = return ()

-- |������������ ���������
ignoreErrors  =  handle (\(_ :: SomeException) -> return ())

-- |������� ����� Channel � �������� � ���� ��������� ������ ��������
newChanWith xs = do c <- newChan
                    writeList2Chan c xs
                    return c

-- |����������� �������
const2 x _ _ = x
const3 x _ _ _ = x
const4 x _ _ _ _ = x

-- |������ ��� ���� ThreadId??
forkIO_ action = forkIO action >> return ()

-- |��������� ����������
foreverM action = do
  action
  foreverM action

-- |����������� ���������, ����������� ����� 'while' � ������� ������
repeat_while inp cond out = do
  x <- inp
  if (cond x)
    then do out x
            repeat_while inp cond out
    else return x

-- |����������� ���������, ����������� repeat-until � �������
repeat_until action = do
  done <- action
  when (not done) $ do
    repeat_until action

-- |����������� ���������, ����������� ���������� �������� �������� size
-- �� ��������� �������� �������� �� ����� chunk �����
doChunks size chunk action =
  case size of
    0 -> return ()
    _ -> do let n = minI size chunk
            action (fromIntegral n)
            doChunks (size-n) chunk action

-- |��������� `action` ��� x, ����� ��� ������ ��������� ������, ������������� �� `action`, � ��� ����� ����������
recursiveM action x  =  action x >>= mapM_ (recursiveM action)

-- |��������� ����������, ���� ����� ������� `cond`, � ���������� � ��������� ������
recursiveIfM cond action x  =  if cond  then recursiveM action x  else (action x >> return ())

-- |��������� �������� `action` ��� ���������� ������ `list` ���������, ���������
-- ������ �����������, ������������ `action` - � �����, ���������� mapM.
-- �� ������������� � ����� ��������� ������������ ������ �� �������� `crit_f` � ������� �� �����,
-- ���� ���� �������� �����������. ������� ������������� ������������ ������ ��������������
-- �������� �� `list`
mapMConditional (init,map_f,sum_f,crit_f) action list = do
  let go []     ys summary = return (reverse ys, [])     -- ��������� ��-�� ���������� ������
  let go (x:xs) ys summary = do
        y <- action x
        let summary  =  sum_f summary (map_f y)
        if (crit_f summary)
          then return (reverse$ y:ys, xs)                -- ��������� �������� ��������
          else go xs (y:ys) summary
  go list [] init

-- |Execute action with background computation
withThread thread  =  bracket (forkIO thread) killThread . const

-- |��������� �������� � ������ ����� � ���������� �������� ���������
bg action = do
  resultVar <- newEmptyMVar
  forkIO (action >>= putMVar resultVar)
  takeMVar resultVar

#ifdef FREEARC_GUI
isGUI = True
#else
isGUI = False
#endif

{-# NOINLINE foreverM #-}
{-# NOINLINE repeat_while #-}
{-# NOINLINE repeat_until #-}
{-# NOINLINE mapMConditional #-}
{-# NOINLINE bg #-}


-- |������������� ������ � ������� ������������� (������������) ���������
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM p  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  p x  >>=  bool (go    accum  xs)
                                           (go (x:accum) xs)

-- |mapMaybe, ����������� � ����� Monad
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  f x  >>=  maybe (      go    accum  xs)
                                            (\r -> go (r:accum) xs)

-- |@firstJust@ takes a list of @Maybes@ and returns the
-- first @Just@ if there is one, or @Nothing@ otherwise.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : ms) = Just x
firstJust (Nothing : ms) = firstJust ms

-- |������� ������ �������� (Just) ��������� ���������� f � ������ ��� Nothing
firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f  =  firstJust . map f

-- |�������� Nothing �� �������� �� ���������
defaultVal = flip fromMaybe

-- |�������� Nothing �� �������� �� ��������� - ��� ������������ ��������
defaultValM = liftM2 defaultVal

-- |������� ���� �� ���� �������� � ����������� �� ���������� ���������
bool onFalse onTrue False  =  onFalse
bool onFalse onTrue True   =  onTrue

-- |if ��� ����. ��������
iif True  onTrue onFalse  =  onTrue
iif False onTrue onFalse  =  onFalse

-- ��������� � ������ ���� �� ���� ������� � ����������� �� ����, ���� �� ��
list onNotNull onNull [] = onNull
list onNotNull onNull xs = onNotNull xs

-- |���������� True, ���� �������� - �� Nothing
maybe2bool (Just _) = True
maybe2bool Nothing  = False

-- |�������� �� Left
isLeft (Left _) = True
isLeft _        = False

-- |������� ��������, ���������� ��������� ���������
deleteIf p = filter (not.p)

-- |������� ��������, ��������������� ������ �� ������ ����������
deleteIfs = deleteIf.anyf

-- |���������� lookup-������
update list a@(key,value)  =  a : [x | x@(k,v)<-list, k/=key]

-- |������ �������� �� ������
changeTo list value  =  lookup value list `defaultVal` value

-- |���������� � ���������� ���� ��������
trace2 s = trace (show s) s

-- |Evaluate list elements
evalList (x:xs) = x `seq` evalList xs
evalList []     = ()

{-
-- Cale Gibbard

A useful little higher order function. Some examples of use:

swing map :: forall a b. [a -> b] -> a -> [b]
swing any :: forall a. [a -> Bool] -> a -> Bool
swing foldr :: forall a b. b -> a -> [a -> b -> b] -> b
swing zipWith :: forall a b c. [a -> b -> c] -> a -> [b] -> [c]
swing find :: forall a. [a -> Bool] -> a -> Maybe (a -> Bool) -- applies each of the predicates to the given value, returning the first predicate which succeeds, if any
swing partition :: forall a. [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])

-}

swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing f = flip (f . flip ($))


-- |Map on functions instead of its' arguments!
map_functions []     x  =  []
map_functions (f:fs) x  =  f x : map_functions fs x

-- |���������, ��� ��� ������� �� ������ ���� True �� (��������� �����) ���������. �����������, ��� swing all
allf x = all_functions x
all_functions []  = const True
all_functions [f] = f
all_functions fs  = and . map_functions fs

-- |���������, ��� ���� ���� ������� �� ������ ��� True �� (��������� �����) ���������. �����������, ��� swing any
anyf x = any_function x
any_function []  = const False
any_function [f] = f
any_function fs  = or . map_functions fs

-- |��������� � ��������� ��������������� ��� ������� �� ������
applyAll []     x = x
applyAll (f:fs) x = applyAll fs (f x)

(f>>>g) x = g(f x)


---------------------------------------------------------------------------------------------------
---- �������� ��� �������� ------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |������� ������ �� ��� ���������, ���������� �������� ��������
split2 :: (Eq a) => a -> [a] -> ([a],[a])
split2 c s  =  (chunk, drop 1 rest)
  where (chunk, rest) = break (==c) s

-- |��������� �� ����� ��� ���� :)
join2 :: [a] -> ([a],[a]) -> [a]
join2 between (a,b) = a++between++b

-- |������� ������ �� ���������, ���������� �������� ��������
split :: (Eq a) => a -> [a] -> [[a]]
split c s =
  let (chunk, rest) = break (==c) s
  in case rest of  []     -> [chunk]
                   _:rest -> chunk : split c rest

-- |��������� ������ ����� � ������ ����� � ������������: "one, two, three"
joinWith :: [a] -> [[a]] -> [a]
joinWith x  =  concat . intersperse x

-- |��������� ������ ����� � ������ �����, ��������� ��� ������ �����������:
-- joinWith2 ", " " and " ["one","two","three","four"]  -->  "one, two, three and four"
joinWith2 :: [a] -> [a] -> [[a]] -> [a]
joinWith2 a b []    =  []
joinWith2 a b [x]   =  x
joinWith2 a b list  =  joinWith a (init list) ++ b ++ last list

-- |��������� x ����� s1 � s2, ���� ��� ������ ��������
between s1 x [] = s1
between [] x s2 = s2
between s1 x s2 = s1++x++s2

-- |�������� ������� ������� ������ ������
quote :: String -> String
quote str  =  "\"" ++ str ++ "\""

-- |������� ������� ������� ������ ������ (���� ��� ����)
unquote :: String -> String
unquote ('"':str) | str>"" && x=='"'  =  xs     where (x:xs) = reverse str
unquote str = str

contains = flip elem

-- |������� n ��������� � ����� ������
dropEnd n  =  reverse . drop n . reverse

-- |������, ���� `s` �������� ���� �� ���� �� ��������� ��������� `set`
s `contains_one_of` set  =  any (`elem` set) s

-- |��������� n ���������
n `lastElems` xs  =  drop (length xs - n) xs

-- |�������� n'� ������� (������ � 0) � ������ `xs` �� `x`
replaceAt n x xs  =  hd ++ x : drop 1 tl
    where (hd,tl) = splitAt n xs

-- |�������� n'� ������� (������ � 0) � ������ `xs` � `x` �� `f x`
updateAt n f xs  =  hd ++ f x : tl
    where (hd,x:tl) = splitAt n xs

-- |�������� � ������ ��� ��������� �������� 'from' �� 'to'
replace from to  =  map (\x -> if x==from  then to  else x)

-- |���� ������ ������ �������� ��������� ������ - ���������� ������� ������ ������, ����� Nothing
startFrom (x:xs) (y:ys) | x==y  =  startFrom xs ys
startFrom [] str                =  Just str
startFrom _  _                  =  Nothing

-- |��������, ��� ������ ���������� ��� ������������� ��������� ���������
beginWith s = isJust . startFrom s
endWith   s = beginWith (reverse s) . reverse

-- |���������� ������� ������ substr � ������ str
tryToSkip substr str  =  (startFrom substr str) `defaultVal` str

-- |���������� ������� ������ substr � ����� str
tryToSkipAtEnd substr str = reverse (tryToSkip (reverse substr) (reverse str))

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the second list is contained, wholy and intact,
-- anywhere within the first.
substr haystack needle  =  any (needle `isPrefixOf`) (tails haystack)

-- |������ ������� ��������� � ������
strPositions haystack needle  =  elemIndices True$ map (needle `isPrefixOf`) (tails haystack)

-- |�������� � ������ `s` ��� ��������� `from` �� `to`
replaceAll from to = repl
  where repl s      | Just remainder <- startFrom from s  =  to ++ repl remainder
        repl (c:cs)                                       =  c : repl cs
        repl []                                           =  []

-- |�������� %1 �� �������� ������
format msg s  =  replaceAll "%1" s msg

-- |�������� %1..%9 �� �������� ������
formatn msg s  =  go msg
  where go ('%':d:rest) | isDigit d = (s !! (digitToInt d-1)) ++ go rest
        go (x:rest)                 = x : go rest
        go ""                       = ""

-- |�������� � ������ `s` ������� `from` �� `to`
replaceAtStart from to s =
  case startFrom from s of
    Just remainder  -> to ++ remainder
    Nothing         -> s

-- |�������� � ������ `s` ������� `from` �� `to`
replaceAtEnd from to s =
  case startFrom (reverse from) (reverse s) of
    Just remainder  -> reverse remainder ++ to
    Nothing         -> s

-- |������� ����������������� ������ ������ �������� � ������ <=255
encode16 (c:cs) | n<256 = [intToDigit(n `div` 16), intToDigit(n `mod` 16)] ++ encode16 cs
                             where n = ord c
encode16 "" = ""

-- |������������ ����������������� ������ ������ �������� � ������ <=255
decode16 (c1:c2:cs) = chr(digitToInt c1 * 16 + digitToInt c2) : decode16 cs
decode16 ""         = ""

-- |����� ������ n ��������� ������ � �������� � ��� more ��� ��������� ����, ��� ���-�� ���� �������
takeSome n more s | (y>[])    = x ++ more
                  | otherwise = x
                  where  (x,y) = splitAt n s

-- |��������� ������ �����/������, �������� � �� �������� ������ ��������� ��� ���-������ ���
right_fill  c n s  =  s ++ replicate (n-length s) c
left_fill   c n s  =  replicate (n-length s) c ++ s
left_justify       =  right_fill ' '
right_justify      =  left_fill  ' '

-- ������� ������� � ������/����� ������ ��� �� ����� ��������
trimLeft  = dropWhile (==' ')
trimRight = reverse.trimLeft.reverse
trim      = trimLeft.trimRight

-- |��������� ������ � ������ �������
strLower = map toLower

-- |�������� ��� ������, ��������� �������
strLowerEq a b  =  strLower a == strLower b

-- |break ������� �� ������� ��������
break1 f (x:xs)  =  mapFst (x:) (break f xs)

-- |���������� �������� �� ��������� ������ ������ ������, ���� �� ����
head1 [] = defaultValue
head1 xs = head xs

-- ������ tail, �������� ����������� �� ������ ������
tail1 [] = []
tail1 xs = tail xs

-- ������ init, �������� ����������� �� ������ ������
init1 [] = []
init1 xs = init xs

-- ������ last, �������� ����������� �� ������ ������
last1 [] = defaultValue
last1 xs = last xs

-- |Map various parts of list
mapHead f []      =  []
mapHead f (x:xs)  =  f x : xs

mapTail f []      =  []
mapTail f (x:xs)  =  x : map f xs

mapInit f []      =  []
mapInit f xs      =  map f (init xs) : last xs

mapLast f []      =  []
mapLast f xs      =  init xs ++ [f (last xs)]

{-# NOINLINE replaceAll #-}
{-# NOINLINE replaceAtEnd #-}



---------------------------------------------------------------------------------------------------
---- �������� ��� �������� ------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Sort list by function result (use Schwarznegian transform)
sortOn  f  =  map snd . sortOn' fst . map (keyval f)

-- |Sort list by function result (don't use Schwarznegian transform!)
sortOn' f  =  sortBy (map2cmp f)

-- |Group list by function result
groupOn f  =  groupBy (map2eq f)

-- |Sort and Group list by function result
sort_and_groupOn  f  =  groupOn f . sortOn  f
sort_and_groupOn' f  =  groupOn f . sortOn' f

-- |������������� ��� �������� (a.b) � ���������� ��������� 'a'
groupFst :: (Ord a) =>  [(a,b)] -> [(a,[b])]
groupFst = map (\xs -> (fst (head xs), map snd xs)) . sort_and_groupOn fst

-- |������� ��������� �� ������
removeDups = removeDupsOn id

-- |��������� ������ �� ������ �������� �� ������ ������ � ���������� ��������� f
removeDupsOn f = map head . sort_and_groupOn f

-- |���������, ��� ��� ���������������� �������� � ������ ������������� ��������� �����������
isAll f []       = True
isAll f [x]      = True
isAll f (x:y:ys) = f x y  &&  isAll f (y:ys)

-- |���������, ��� ���� �� ��� �����-������ ���������������� �������� � ������ ������������� ��������� �����������
isAny f []       = False
isAny f [x]      = False
isAny f (x:y:ys) = f x y  ||  isAny f (y:ys)

-- |Check that list is sorted by given field/critery
isSortedOn f  =  isAll (<=) . map f

-- |Check that all elements in list are equal by given field/critery
isEqOn f      =  isAll (==) . map f

-- |Find maximum element by given comparison critery
maxOn f (x:xs) = go x xs
  where go x [] = x
        go x (y:ys) | f x > f y  =  go x ys
                    | otherwise  =  go y ys

-- |Merge two lists, sorted by `cmp`, in one sorted list
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp xs [] = xs
merge cmp [] ys = ys
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

-- |������� ������ �� `numGroups` ���������� � ������������ �� ���������, ������������ `crit_f`
partitionList numGroups crit_f list =
  elems $ accumArray (flip (:)) [] (0, numGroups-1) (map (keyval crit_f) (reverse list))

-- partitionList numGroups crit_f list =
--   let xs = map (keyval crit_f) list
--       go 0 [] all = all
--       go n list prev = let (this, next) = partition (\(a,b) -> a==n-1) list
--                        in go (n-1) next (map snd this:prev)
--   in go numGroups xs []

-- |������� ������ �� ������ � ������������ � ����������� �� ������ `groups`:
--   splitList [(=='a'), (=='c')] 2 "cassa"  ->  ["aa","c","ss"]
--
splitList groups default_group filelist =
  let go [] filelist sorted  =  replaceAt default_group filelist (reverse sorted)
      go (group:groups) filelist sorted =
        let (found, notfound)  =  partition group filelist
        in go groups notfound (found:sorted)
  in go groups filelist []

-- |����� ����� ������� ��������� �� ������ `groups`, �������� ������������� �������� `value`
findGroup groups default_group value  =  (findIndex ($ value) groups) `defaultVal` default_group

-- Utility functions for list operations
keyval  f x    =  (f x, x)                -- |Return pair containing computed key and original value
map2cmp f x y  =  (f x) `compare` (f y)   -- |Converts "key_func" to "compare_func"
map2eq  f x y  =  (f x) == (f y)          -- |Converts "key_func" to "eq_func"


-- |����������� ��������� ������
recursive :: ([a]->(b,[a])) -> [a] -> [b]
recursive f list  =  list &&& (x:recursive f xs)   where (x,xs) = f list

-- |������� ������ �� ���������, ����� ������� ������������ ������� ������� `len_f` �� ������� ������
splitByLen :: ([a]->Int) -> [a] -> [[a]]
splitByLen len_f  =  recursive (\xs -> splitAt (len_f xs) xs)

-- |��� ������� �������� ������ ���� ���������� � ��������� `xs` � ������������ � ���
splitByLens (len:lens) list  =  (x:splitByLens lens xs)    where (x,xs) = splitAt len list
splitByLens []         []    =  []

-- |���������� ����� ���������� �������� ������, ���������������� ���������������� �������,
-- �������� "groupLen (fiSize) (+) (<16*mb) files" ���������� ����� ���������� �������� ������,
-- ���������� ����� ��������� ������� �� ����� 16 ��������
groupLen mapper combinator tester  =  length . takeWhile tester . scanl1 combinator . map mapper

-- |���������� ���������� span � break: spanBreak isDigit "100a10b2c" = ("100a", "10b2c")
spanBreak crit xs  = let (s1,tail1) = span  crit xs
                         (s2,tail2) = break crit tail1
                     in (s1++s2, tail2)

-- |������� ������ �� ������, ��������� ������� - ��������, ���������� �������� 'crit'
makeGroups              :: (a -> Bool) -> [a] -> [[a]]
makeGroups crit []      =  []
makeGroups crit (x:xs)  =  (x:ys) : makeGroups crit zs
                             where (ys,zs) = break crit xs

-- |������� ������ �� ������, ���������� ����������, ����������� �������� 'crit':
-- splitOn even [1,2,4,8,3,5,7] == [[1],[2],[4],[8],[3,5,7]]
splitOn crit []  =  []
splitOn crit xs  =  (not(null ys)  &&&  (ys :))
                    (not(null zs)  &&&  ([head zs] : splitOn crit (tail zs)))
                      where (ys,zs) = break crit xs

-- |������� � ������ ��������� �� ��������� ��������. O(n^2), ���� ��������� ������� ��������� � ������
keepOnlyFirstOn f [] = []
keepOnlyFirstOn f (x:xs) = x : keepOnlyFirstOn f (filter (\a -> f x /= f a) xs)

-- |�������� � ������ ������ ��������� �� ���������� �� ��������� ��������
keepOnlyLastOn f = reverse . keepOnlyFirstOn f . reverse

-- |������� �������� � ��������� �������� �� ������
deleteElems = go 0
  where go n xs [] = xs  -- ������� ������ ������
        go n (x:xs) iis@(i:is) | n<i  = x:go (n+1) xs iis  -- �� ��� �� ����� �� i-�� ��������
                               | n==i =   go (n+1) xs is   -- ����� - �������!


-- |���������� ������������ ������ ����� � ������ ����������:
-- 1,2,3,10,21,22 -> (1,3),(10,10),(21,22)
makeRanges (x:y:zs) | x+1==y    =  makeRanges1 x (y:zs)
                    | otherwise =  (x,x) : makeRanges (y:zs)
makeRanges [x]                  =  [(x,x)]
makeRanges []                   =  []

-- ��������������� ����������� ��� makeRanges
makeRanges1 start (x:y:zs) | x+1==y    =  makeRanges1 start (y:zs)
                           | otherwise =  (start,x) : makeRanges (y:zs)
makeRanges1 start [x]                  = [(start,x)]


{-# NOINLINE partitionList #-}
{-# NOINLINE splitList #-}


---------------------------------------------------------------------------------------------------
---- �������� � ��������� -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |���������� ������ � 0-based array
listArray0 list  =  listArray (0,length(list)-1) list

-- |����� �����. � ����. ������� � ������ ��� � ������� �� ��� ������,
populateArray defaultValue castValue pairs =
  accumArray (\a b -> castValue b) defaultValue (minimum indexes, maximum indexes) pairs
  where indexes = map fst pairs


---------------------------------------------------------------------------------------------------
---- �������� � tuples ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- �������� ��� tuple/2
mapFst    f (a,b)  =  (f a,   b)
mapSnd    f (a,b)  =  (  a, f b)
mapFstSnd f (a,b)  =  (f a, f b)
map2      (f,g) a  =  (f a, g a)
map5      (f1,f2,f3,f4,f5) a  =  (f1 a,f2 a,f3 a,f4 a,f5 a)
mapFsts = map . mapFst
mapSnds = map . mapSnd
map2s   = map . map2

-- |����� ������ �������� ��� � ������ � �������� ���� (�����) ������ �������
concatSnds xs = (fst (head xs), concatMap snd xs)

-- �������� ��� tuple/3
fst3 (a,_,_)    =  a
snd3 (_,a,_)    =  a
thd3 (_,_,a)    =  a
map3 (f,g,h) a  =  (f a, g a, h a)


---------------------------------------------------------------------------------------------------
---- �������� ������� ���������� ------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 0 =:, +=, -=, ++=, =::, .=, .<-, <<=, <=>

-- Simple variables
class Variable v a | v->a where
  new  :: a -> IO v
  val  :: v -> IO a
  (=:) :: v -> a -> IO ()
  (.=) :: v -> (a->a) -> IO ()
  (=::) :: v -> IO a -> IO ()
  (.<-) :: v -> (a->IO a) -> IO ()
  -- Default implementations
  a.=f = do x<-val a; a=:f x
  a=::b = (a=:) =<< b
  a.<-f = do x<-val a>>=f; a=:x

ref = newIORef
instance Variable (IORef a) a where
  new = newIORef
  val = readIORef
  a=:b = writeIORef a b
  a.=b = modifyIORef a b
  a.<-b = modifyIORefIO a b

mvar = newMVar
instance Variable (MVar a) a where
  new = newMVar
  val = readMVar
  a=:b = swapMVar a b >> return ()
  a.=b = modifyMVar_ a (return.b)
  a.<-b = modifyMVar_ a b

a+=b = a.=(\a->a+b)
a-=b = a.=(\a->a-b)
a++=b = a.=(\a->a++b)
a<=>b = do x <- val a; a =: b; return x
withRef init  =  with' (ref init) val


-- Accumulation lists
newtype AccList a = AccList [a]
newList   = ref$ AccList []
a<<=b     = a .= (\(AccList x) -> AccList$ b:x)
pushList  = (<<=)
listVal a = val a >>== (\(AccList x) -> reverse x)
withList  =  with' newList listVal


-- |�������� �������� � ������, �������� �� ������ IORef
addToIORef :: IORef [a] -> a -> IO ()
addToIORef var x  =  var .= (x:)

-- |������������ ��������, ���������� �� ������ IORef, � ���������,
-- � �������� �� ��� ����� ����� ��������, ������������ ���� ����������
modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO var action = do
  readIORef var  >>=  action  >>=  writeIORef var

-- |��� ���� �������� ����������� ���������
with' init finish action  =  do a <- init;  action a;  finish a

-- |��������� �������� � ���������� � ��������� ������ ���������� init/finish ��������
inside init finish action  =  do init;  x <- action;  finish; return x

-- |��������� "add key" c ������������ �����������
lookupMVarCache mvar add key = do
  modifyMVar mvar $ \assocs -> do
    case (lookup key assocs) of
      Just value -> return (assocs, value)
      Nothing    -> do value <- add key
                       return ((key,value):assocs, value)


-- JIT-���������� ���������������� ������ � ������ �� ������� ������������
newJIT init        = ref (Left init)
delJIT a    finish = whenRightM_ (val a) finish
valJIT a           = do x <- val a
                        case x of
                          Left init -> do x<-init; a=:Right x; return x
                          Right x   -> return x

withJIT init finish action = do a <- newJIT init;  action a  `finally`  delJIT a finish


---------------------------------------------------------------------------------------------------
---- ��������� ���������� � �������� ��� ������ ---------------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 6 +:, -:
ptr+:n   = ptr `plusPtr` (fromIntegral n)
ptr-:buf = fromIntegral  (ptr `minusPtr` buf)
copyBytesI dst src len  =  copyBytes dst src (i len)
minI a b                =  i$ min (i a) (i b)
maxI a b                =  i$ max (i a) (i b)
clipToMaxInt            =  i. min (i (maxBound::Int))
atLeast                 =  max
i                       =  fromIntegral
clipTo low high         =  min high . max low
divRoundUp   x chunk    = ((x-1) `div` i chunk) + 1
roundUp      x chunk    = divRoundUp x chunk * i chunk
divRoundDown x chunk    = x `div` i chunk
roundDown    x chunk    = divRoundDown x chunk * i chunk
roundTo      x chunk    = i (((((toInteger(x)*2) `divRoundDown` chunk)+1) `divRoundDown` 2) * i chunk) `asTypeOf` x


---------------------------------------------------------------------------------------------------
---- ������������� ������ � ����������� ������ ----------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |������������� ������ � ����������� ������ � ������������� ���������� ������
--   heapsize     - ������ ������
--   aBUFFER_SIZE - ������������ ������ ��������������� �����
--   aALIGN       - ��� ���������� ����� ������������� �� �������, ������� ����� �����
--   returnBlock  - ��������� ��������� ������, ������������� ������������. ����������,
--                    ����� ������ ���������� ������������ ��� �������������� ���������� �������
--
allocator heapsize aBUFFER_SIZE aALIGN returnBlock = do
  let aHEAP_START = 0          -- ������ ������, ������ ���� = 0
      aHEAP_END   = heapsize   -- ����� ������

  start <- ref aHEAP_START     -- ��������� ������ ���������� ����� � ������
  end   <- ref aHEAP_END       -- ��������� ����� ���������� �����
                               -- ���� ��� ��������� �����, �� ���������� ����� ���
#if 0
  let debug = putStr         -- ���������� ������

  let printStats s = do      -- ���������� ��������� ������ ��� �������
        astart <- val start
        aend <- val end
        debug$ left_justify 48 s++"STATE start:"++show astart++", end:"++show aend++", avail:"++show ((aend-astart) `mod` aHEAP_END)++"\n"

  debug "\n"
#else
  let debug      = return
      printStats = return
#endif

  -- ��������� �������� �� ��������� ��������, ������� aALIGN
  let align n  =  (((n-1) `div` aALIGN) + 1) * aALIGN

  -- ���������� ����� �����: >=n, ������������ �� aALIGN � �������� ��� ������� aBUFFER_SIZE ���� �� ����� ������
  let nextAvail n = if (aHEAP_END-aligned<aBUFFER_SIZE)
                      then aHEAP_END
                      else aligned
                    where aligned = align n

  -- ���������� ���������� ��������� ������ � ������
  let available = do
        astart <- val start
        aend   <- val end
        if (astart<=aend) then
           return (aend-astart)
         else if (astart<aHEAP_END) then
           return (aHEAP_END-astart)
         else do
           -- ��������� ��������� ������ ��������� ������ �� ������ ������
           start =: aHEAP_START
           debug "===================================\n"
           printStats ""
           available

  -- ��������� ������������ ����� ������ � �������� ��� ������������
  let waitReleasingMemory = do
        (addr,size) <- returnBlock
        astart <- val start
        aend   <- val end
        unless (addr == aend || (addr==aHEAP_START && (aHEAP_END-aend<aBUFFER_SIZE)))$  fail "addToAvail!"
        let new_end = nextAvail(addr+size)
        if new_end == astart
          then do start=:aHEAP_START; end=:aHEAP_END -- now all memory is free
          else end =: new_end
        printStats$ "*** returned buf:"++show addr++" size:"++show size++"   "

  -- �������� ��������� ���� ������� aBUFFER_SIZE. ���� ��������� ������ ��� - ��������
  --   ����������� ������������ ���������� ���������� ������ ������
  let getBlock = do
        avail <- available
        if (avail >= aBUFFER_SIZE) then do
           block <- val start
           start =: error "Block not shrinked"
           return block
         else do
           waitReleasingMemory
           getBlock

  -- ��������� ���������� ���� �� ������� `size`. ������ ���� ����������� ������� ����� getBlock
  let shrinkBlock block size = do
        astart <- val start
        --unless (astart == block)$      fail "Tryed to shrink another block"
        unless (size <= aBUFFER_SIZE)$  fail "Growing instead of shrinking :)"
        start =: nextAvail(block+size)
        printStats$ "getBlock buf:"++show block++", size: "++show aBUFFER_SIZE++" --> "++show size++"    "

  -- ���������� ��������� � ������������� ������������ ������
  return (getBlock, shrinkBlock)


-- |����������� ���������, ������������ ���� ������ `heap`.
-- ����������� �������, � �������� �������� ������� `allocator`
memoryAllocator heap size chunksize align returnBlock = do
  let returnBlock2            =  do (buf,len) <- returnBlock; return (buf-:heap, len)
  (getBlock2, shrinkBlock2)  <-  allocator size chunksize align returnBlock2
  let getBlock                =  do block <- getBlock2; return (heap+:block)
      shrinkBlock buf len     =  do shrinkBlock2 (buf-:heap) len
  return (getBlock, shrinkBlock)


---------------------------------------------------------------------------------------------------
---- ��������� ���������� ���������.                                                           ----
---- todo: #define FULL_REGEXP �������� ������������� ����������� ���. ���������: r[0-9][0-9]  ----
---------------------------------------------------------------------------------------------------

-- |���������������� ������������� ����������� ���������                            ������
data RegExpr = RE_End                     -- ����� �����                            ""
             | RE_Anything                -- ����� ������                           "*"
             | RE_AnyStr  RegExpr         -- '*', ����� ������� ����� ��� '*'       '*':"bc*"
             | RE_FromEnd RegExpr         -- ��������� ������������ RE ����� ������ '*':"bc"
             | RE_AnyChar RegExpr         -- ����� ������, ����� RE                 '?':"bc"
             | RE_Char    Char RegExpr    -- �������� ������, ����� RE              'a':"bc"

-- |���������, ��� ������ �������� ���� �� ��������,
-- ������� ����������� �������� � ���������� ����������
is_wildcard s  =  s `contains_one_of` "?*"

-- |�������������� ��������� ������������� ����������� ��������� � ��������� RegExpr
compile_RE s  =  case s of
  ""                         -> RE_End
  "*"                        -> RE_Anything
  '*':cs | cs `contains` '*' -> RE_AnyStr   (compile_RE  cs)
         | otherwise         -> RE_FromEnd  (compile_RE$ reverse s)
  '?':cs                     -> RE_AnyChar  (compile_RE  cs)
  c  :cs                     -> RE_Char   c (compile_RE  cs)

-- |��������� ������������ ������ ����������������� ����������� ���������
match_RE r = case r of
  RE_End        -> null
  RE_Anything   -> const True
  RE_AnyStr   r -> let re = match_RE r in \s -> any re (tails s)
  RE_FromEnd  r -> let re = match_RE r in re . reverse
  RE_AnyChar  r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     _:xs -> re xs
  RE_Char   c r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     x:xs -> x==c && re xs

-- |��������� ������������ ������ `s` ����������� ��������� `re`
match re {-s-}  =  match_RE (compile_RE re) {-s-}

-- Perl-like names for matching routines
infix 4 ~=, !~
(~=)    = flip match
a !~ b  = not (a~=b)
