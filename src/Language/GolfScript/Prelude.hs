{-# LANGUAGE ViewPatterns #-}
{- | The standard set of built-in functions included with GolfScript. -}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.List.Split
import Data.Accessor

-- | Two values popped off the stack, coerced to the same type. For @Foos x y@,
-- the original stack looked like @[y, x, ...]@.
data Coerced m
  = Ints Integer Integer
  | Arrs [Val m] [Val m]
  | Strs String String
  | Blks [Do m] [Do m]
  deriving (Eq, Ord, Show, Read)

-- | Two values popped off the stack, placed in a standard type-priority order.
-- For @FooFoo x y@, the original stack looked like @[y, x, ...]@.
data Ordered m
  = IntInt Integer Integer
  | IntArr Integer [Val m]
  | IntStr Integer String
  | IntBlk Integer [Do m]
  | ArrArr [Val m] [Val m]
  | ArrStr [Val m] String
  | ArrBlk [Val m] [Do m]
  | StrStr String String
  | StrBlk String [Do m]
  | BlkBlk [Do m] [Do m]
  deriving (Eq, Ord, Show, Read)

type S m = StateT (Golf m) m

-- | Packages a state function as a value that can be assigned to a variable.
prim :: (Monad m) => S m () -> Val m
prim s = Blk [Prim $ P $ execStateT s]

--
-- Helper functions
--

spush :: (Monad m) => Val m -> S m ()
spush x = modify (push x)

spop' :: (Monad m) => S m (Val m)
spop' = gets pop >>=
  maybe (error "spop: empty stack") (\(x, g) -> put g >> return x)

spop :: (Monad m) => S m (Maybe (Val m))
spop = gets pop >>= maybe (return Nothing) (\(x, g) -> put g >> return (Just x))

-- | False values are the number 0, and the empty array/string/block.
bool :: Val m -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk []]

unbool :: Bool -> Val m
unbool b = Int $ if b then 1 else 0

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ f >=> \s' -> return ((), s')

-- | Pop a value off the stack and use it. If the stack is empty, does nothing.
unary :: (Monad m) => (Val m -> S m ()) -> S m ()
unary f = spop >>= maybe (return ()) f

-- | Pop two values off the stack and use them. If there's only one value on the
-- stack, it's popped back on and nothing is executed.
binary :: (Monad m) => (Val m -> Val m -> S m ()) -> S m ()
binary f = unary $ \y -> spop >>= maybe (spush y) (\x -> f x y)

-- | Pop three values off the stack and use them. If there's only one or two
-- values on the stack, they're popped back on and nothing is executed.
ternary :: (Monad m) => (Val m -> Val m -> Val m -> S m ()) -> S m ()
ternary f = binary $ \y z -> spop >>= maybe (spush y >> spush z) (\x -> f x y z)

strToArr :: String -> [Val m]
strToArr = map $ Int . c2i

arrToStr :: [Val m] -> String
arrToStr = mapMaybe $ \x -> case x of
  Int i -> Just $ i2c i
  _ -> Nothing

-- | Runs a command sequence, then pops a value off and evaluates it for truth.
predicate :: (Monad m) => [Do m] -> S m Bool
predicate xs = modifyM (runs xs) >> liftM (maybe False bool) spop

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

c2i :: Char -> Integer
c2i = fromIntegral . fromEnum

i2c :: Integer -> Char
i2c = toEnum . fromIntegral

coerce :: (Monad m) => (Coerced m -> S m ()) -> S m ()
coerce f = binary $ \x y -> f $ case (x, y) of
  (Int a, Int b) -> Ints a b
  (Int _, Arr b) -> Arrs [x] b
  (Int a, Str b) -> Strs (show a) b
  (Int _, Blk b) -> Blks [Push x] b
  (Arr a, Int _) -> Arrs a [y]
  (Arr a, Arr b) -> Arrs a b
  (Arr a, Str b) -> Strs (arrToStr a) b
  (Arr _, Blk _) -> undefined -- TODO
  (Str a, Int b) -> Strs a (show b)
  (Str a, Arr b) -> Strs a (arrToStr b)
  (Str a, Str b) -> Strs a b
  (Str a, Blk b) -> Blks (parse $ scan a) b
  (Blk a, Int _) -> Blks a [Push y]
  (Blk _, Arr _) -> undefined -- TODO
  (Blk a, Str b) -> Blks a (parse $ scan b)
  (Blk a, Blk b) -> Blks a b

order :: (Monad m) => (Ordered m -> S m ()) -> S m ()
order f = binary $ \x y -> f $ case (x, y) of
  (Int a, Int b) -> IntInt a b
  (Int a, Arr b) -> IntArr a b
  (Int a, Str b) -> IntStr a b
  (Int a, Blk b) -> IntBlk a b
  (Arr a, Int b) -> IntArr b a
  (Arr a, Arr b) -> ArrArr a b
  (Arr a, Str b) -> ArrStr a b
  (Arr a, Blk b) -> ArrBlk a b
  (Str a, Int b) -> IntStr b a
  (Str a, Arr b) -> ArrStr b a
  (Str a, Str b) -> StrStr a b
  (Str a, Blk b) -> StrBlk a b
  (Blk a, Int b) -> IntBlk b a
  (Blk a, Arr b) -> ArrBlk b a
  (Blk a, Str b) -> StrBlk b a
  (Blk a, Blk b) -> BlkBlk a b

--
-- The built-ins
--

-- | @[@ starts an array \"literal\"
lb :: (Monad m) => S m ()
lb = modify $ (brackets ^: (0 :))

-- | @]@ ends an array \"literal\"
rb :: (Monad m) => S m ()
rb = modify $ \g -> case g ^. brackets of
  [] -> stack ^: (\s -> [Arr $ reverse s]) $ brackets ^= [] $ g
  b : bs -> case splitAt b $ g ^. stack of
    (stkl, stkr) -> stack ^= (Arr $ reverse stkl) : stkr $ brackets ^= bs $ g

-- | @.@ duplicates the top value, by 1 pop and 2 pushes
dot :: (Monad m) => S m ()
dot = unary $ \x -> spush x >> spush x

-- | @~@ bitwise not (int), eval (blk\/str), push each (arr)
tilde :: (Monad m) => S m ()
tilde = unary $ \x -> case x of
  Int i -> spush $ Int $ complement i
  Arr a -> mapM_ spush a
  Blk b -> modifyM $ runs b
  Str s -> modifyM $ runs $ parse $ scan s

-- | @!@ boolean not: if in {@0@, @[]@, @\"\"@, @{}@}, push 1. else push 0.
bang :: (Monad m) => S m ()
bang = unary $ \x -> spush $ Int $ if bool x then 0 else 1

-- | @\@@ bring third value to top: @[z, y, x, ...]@ becomes @[x, z, y, ...]@
at :: (Monad m) => S m ()
at = ternary $ \x y z -> spush y >> spush z >> spush x

-- | @\\@ swap top two elements: @[y, x, ...]@ becomes @[x, y, ...]@
backslash :: (Monad m) => S m ()
backslash = binary $ \x y -> spush y >> spush x

-- | @;@ pop and discard top element
semicolon :: (Monad m) => S m ()
semicolon = spop >> return ()

-- | @,@ make @[0..n]@ (int), length (arr\/str), filter arr\/str by key (blk)
comma :: (Monad m) => S m ()
comma = unary $ \x -> case x of
  Int i -> spush $ Arr $ map Int [0 .. i-1]
  Arr a -> spush $ Int $ fromIntegral $ length a
  Str s -> spush $ Int $ fromIntegral $ length s
  Blk b -> spop >>= \mb -> case mb of -- take array a, then: filterBy b a
    Just (Arr a) -> filterM (\v -> spush v >> predicate b) a >>= spush . Arr
    Just (Str s) -> filterM (\v -> spush v >> predicate b) (strToArr s) >>=
      spush . Str . arrToStr
    Just (Int _) -> undefined -- .rb error
      -- maybe, filter single int?
    Just (Blk _) -> undefined -- TODO: ???
    Nothing -> spush x -- .rb error

-- | @(@ decrement (int), uncons from left (arr\/str)
lp :: (Monad m) => S m ()
lp = unary $ \x -> case x of
  Int i -> spush $ Int $ i - 1
  Arr (v : vs) -> spush (Arr vs) >> spush v
  Str (c : cs) -> spush (Str cs) >> spush (Int $ c2i c)
  _ -> spush x

-- | @)@ increment (int), uncons from right (arr\/str)
rp :: (Monad m) => S m ()
rp = unary $ \x -> case x of
  Int i -> spush $ Int $ i + 1
  Arr (unsnoc -> Just (vs, v)) -> spush (Arr vs) >> spush v
  Str (unsnoc -> Just (cs, c)) -> spush (Str cs) >> spush (Int $ c2i c)
  _ -> spush x

-- | @`@ uneval: convert a value to the code which generates that value
backtick :: (Monad m) => S m ()
backtick = unary $ \x -> spush $ Str $ uneval [Push x]

-- | @$@ copy nth item from stack (int), sort (arr\/str), take str\/arr and
-- sort by mapping (blk)
dollar :: (Monad m) => S m ()
dollar = unary $ \x -> case x of
  Int i -> gets (^. stack) >>= \stk -> case lookup i (zip [0..] stk) of
    Nothing -> return ()
    Just v  -> spush v
  Arr a -> spush $ Arr $ sort a
  Str s -> spush $ Str $ sort s
  Blk b -> unary $ \y -> case y of
    Arr a -> sortOnM f a >>= spush . Arr
    Str s -> sortOnM (f . Int . c2i) s >>= spush . Str
    _ -> undefined
    where f z = spush z >> modifyM (runs b) >> spop'

sortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = mapM f xs >>= \ys ->
  return $ map fst $ sortBy (comparing snd) $ zip xs ys

-- | @+@ coerce: add (ints), concat (arrs\/strs\/blks)
plus :: (Monad m) => S m ()
plus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x + y
  Arrs x y -> spush $ Arr $ x ++ y
  Strs x y -> spush $ Str $ x ++ y
  Blks x y -> spush $ Blk $ x ++ [Get " "] ++ y

-- | @|@ coerce: bitwise or (ints), setwise or (arrs\/strs\/blks)
pipe :: (Monad m) => S m ()
pipe = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .|. y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ parse $ scan $ op (uneval x) (uneval y)
  where op x y = nub $ union x y

-- | @|@ coerce: bitwise and (ints), setwise and (arrs\/strs\/blks)
ampersand :: (Monad m) => S m ()
ampersand = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .&. y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ parse $ scan $ op (uneval x) (uneval y)
  where op x y = nub $ intersect x y

-- | @^@ coerce: bitwise xor (ints), setwise xor (arrs\/strs\/blks)
caret :: (Monad m) => S m ()
caret = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ xor x y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ parse $ scan $ op (uneval x) (uneval y)
  where op x y = nub $ union x y \\ intersect x y

-- | @^@ coerce: subtract (ints), setwise difference (arrs\/strs\/blks)
minus :: (Monad m) => S m ()
minus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x - y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ parse $ scan $ op (uneval x) (uneval y)
  where op x y = filter (`notElem` y) x

-- | @*@ order: multiply (int*int), run n times (int*blk), multiply and join
-- (int*seq), join with separator (seq*seq), fold (seq*blk)
star :: (Monad m) => S m ()
star = order $ \o -> case o of
  -- multiply
  IntInt x y -> spush $ Int $ x * y
  -- concat n copies of seq
  IntArr x y -> spush $ Arr $ concat $ genericReplicate x y
  IntStr x y -> spush $ Str $ concat $ genericReplicate x y
  -- run a block n times
  IntBlk x y -> modifyM $ runs $ concat $ genericReplicate x y
  -- join two sequences
  ArrArr _ _ -> undefined -- TODO: join
  ArrStr _ _ -> undefined -- TODO: join
    -- Note that 'str arr *' will reorder to 'arr str *'.
  StrStr x y -> spush $ Str $ intercalate y $ map (:"") x
  -- fold
  ArrBlk _ _ -> undefined -- TODO: fold
  StrBlk _ _ -> undefined -- TODO: fold
  -- ???
  BlkBlk _ y -> mapM_ (spush . Int . c2i) $ uneval y
    -- convert y to str, push each char int to stack ???
    -- {anything}{abc}* ==> [97 98 99]
    -- probably a bug, but we're gonna copy it :D

slash :: (Monad m) => S m ()
slash = order $ \o -> case o of
  -- int/int: divide
  IntInt x y -> spush $ Int $ div x y
  -- int/seq: split seq into chunks of n elements
  IntArr x y -> spush $ Arr $ map Arr $ chunksOf (fromIntegral x) y
  IntStr x y -> spush $ Arr $ map Str $ chunksOf (fromIntegral x) y
  -- seq/seq: split x on occurrences of y
  ArrArr x y -> spush $ Arr $ map Arr $ splitOn y x
  ArrStr x y -> spush $ Arr $ map Arr $ splitOn (strToArr y) x
    -- Note that 'str arr /' will reorder to 'arr str /'.
  StrStr x y -> spush $ Arr $ map Str $ splitOn y x
  -- seq/blk: run block for each elem in seq
  ArrBlk x y -> forM_ x $ \v -> spush v >> modifyM (runs y)
  StrBlk x y -> forM_ (strToArr x) $ \v -> spush v >> modifyM (runs y)
  -- blk/blk: TODO unfold
  BlkBlk _ _ -> undefined
  -- TODO: ???
  IntBlk _ _ -> undefined

percent :: (Monad m) => S m ()
percent = order $ \o -> case o of
  -- int/int: modulo
  IntInt x y -> spush $ Int $ mod x y
  -- int/seq: select elems from y whose index mod x is 0
  IntArr x y -> if x < 0
    then spush $ Arr $ map head $ chunksOf (fromIntegral $ abs x) $ reverse y
    else spush $ Arr $ map head $ chunksOf (fromIntegral x) y
  IntStr x y -> if x < 0
    then spush $ Str $ map head $ chunksOf (fromIntegral $ abs x) $ reverse y
    else spush $ Str $ map head $ chunksOf (fromIntegral x) y
  -- seq/seq: split x on occurrences of y, but get rid of empty segments
  ArrArr x y -> spush $ Arr $ map Arr $ filter (not . null) $ splitOn y x
  ArrStr x y -> spush $ Arr $ map Arr $ filter (not . null) $ splitOn (strToArr y) x
  StrStr x y -> spush $ Arr $ map Str $ filter (not . null) $ splitOn y x
  -- seq/blk: map elements
  ArrBlk x y -> lb >> forM_ x (\v -> spush v >> modifyM (runs y)) >> rb
  StrBlk x y -> lb >> forM_ (strToArr x) (\v -> spush v >> modifyM (runs y)) >> rb
  -- TODO: ???
  IntBlk _ _ -> undefined
  BlkBlk _ _ -> undefined

less :: (Monad m) => S m ()
less = order $ \o -> case o of
  IntInt x y -> spush $ unbool $ x < y
  ArrArr x y -> spush $ unbool $ x < y
  StrStr x y -> spush $ unbool $ x < y
  BlkBlk x y -> spush $ unbool $ uneval x < uneval y
  _ -> undefined -- TODO

greater :: (Monad m) => S m ()
greater = order $ \o -> case o of
  IntInt x y -> spush $ unbool $ x > y
  ArrArr x y -> spush $ unbool $ x > y
  StrStr x y -> spush $ unbool $ x > y
  BlkBlk x y -> spush $ unbool $ uneval x > uneval y
  _ -> undefined -- TODO

equal :: (Monad m) => S m ()
equal = order $ \o -> case o of
  -- For same types, test for equal.
  IntInt x y -> spush $ unbool $ x == y
  ArrArr x y -> spush $ unbool $ x == y
  StrStr x y -> spush $ unbool $ x == y
  BlkBlk x y -> spush $ unbool $ uneval x == uneval y
  ArrStr x y -> spush $ unbool $ x == strToArr y
  StrBlk x y -> spush $ unbool $ x == uneval y
  -- For int and sequence, get at index.
  IntArr x y -> maybe (return ()) spush $ lookup x $ zip [0..] y
  IntStr x y -> maybe (return ()) (spush . Int . c2i) $
    lookup x $ zip [0..] y
  IntBlk x y -> maybe (return ()) (spush . Int . c2i) $
    lookup x $ zip [0..] $ uneval y
  -- TODO: ???
  ArrBlk _ _ -> undefined

question :: (Monad m) => S m ()
question = order $ \o -> case o of
  -- For two ints, exponent.
  IntInt x y -> spush $ Int $ x ^ y
  -- For int and seq, find element and push index.
  IntArr x y -> spush $ Int $ fromMaybe (-1) $ lookup (Int x) $ zip y [0..]
  IntStr x y -> spush $ Int $ fromMaybe (-1) $ lookup (i2c x) $ zip y [0..]
  -- For seq and blk, find element and push index.
  _ -> undefined -- TODO

doWhile :: (Monad m) => S m ()
doWhile = unary $ \x -> case x of
  Blk b -> go where
    go = predicate b >>= \p -> if p then go else return ()
  _ -> undefined -- TODO

--
-- And finally, the initial state with built-in functions
--

prelude :: (Monad m) => Golf m
prelude = variables ^= var $ empty where
  var = M.fromList
    [ ("[", prim lb)
    , ("]", prim rb)
    , (".", prim dot)
    , ("~", prim tilde)
    , ("!", prim bang)
    , ("@", prim at)
    , ("\\", prim backslash)
    , (";", prim semicolon)
    , (",", prim comma)
    , ("(", prim lp)
    , (")", prim rp)
    , ("`", prim backtick)
    , ("$", prim dollar)
    , ("+", prim plus)
    , ("|", prim pipe)
    , ("&", prim ampersand)
    , ("^", prim caret)
    , ("-", prim minus)
    , ("*", prim star)
    , ("/", prim slash)
    , ("%", prim percent)
    , ("<", prim less)
    , (">", prim greater)
    , ("=", prim equal)
    , ("?", prim question)
    , ("and", Blk $ parse $ scan "1$if")
    , ("or", Blk $ parse $ scan "1$\\if")
    , ("xor", Blk $ parse $ scan "\\!!{!}*")
    , ("n", Blk [Push $ Str "\n"])
    , ("do", prim doWhile)
    ]
