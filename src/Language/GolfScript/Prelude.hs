{-# LANGUAGE ViewPatterns #-}
{- | The standard set of built-in functions included with GolfScript. -}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.List.Split
import Data.Accessor
import System.Random

-- | Two values popped off the stack, coerced to the same type. For @Foos x y@,
-- the original stack looked like @[y, x, ...]@.
data Coerced m
  = Ints Integer Integer
  | Arrs [Val m] [Val m]
  | Strs String String
  | Blks (Block m) (Block m)
  deriving (Eq, Ord, Show, Read)

-- | Two values popped off the stack, placed in a standard type-priority order.
-- For @FooFoo x y@, the original stack looked like @[y, x, ...]@.
data Ordered m
  = IntInt Integer Integer
  | IntArr Integer [Val m]
  | IntStr Integer String
  | IntBlk Integer (Block m)
  | ArrArr [Val m] [Val m]
  | ArrStr [Val m] String
  | ArrBlk [Val m] (Block m)
  | StrStr String String
  | StrBlk String (Block m)
  | BlkBlk (Block m) (Block m)
  deriving (Eq, Ord, Show, Read)

type S m = StateT (Golf m) m

-- | Packages a state function as a value that can be assigned to a variable.
prim :: (Monad m) => S m () -> Val m
prim s = Blk $ doBlock [Prim $ P $ execStateT s]

--
-- Helper functions
--

spush :: (Monad m) => Val m -> S m ()
spush x = modify (push x)

spop' :: (Monad m) => S m (Val m)
spop' = gets pop >>=
  maybe (error "spop': empty stack") (\(x, g) -> put g >> return x)

spop :: (Monad m) => S m (Maybe (Val m))
spop = gets pop >>= maybe (return Nothing) (\(x, g) -> put g >> return (Just x))

-- | Returns the top value of the stack, without doing a pop operation.
-- Bracket boundaries aren't changed.
top :: (Monad m) => S m (Val m)
top = gets (^. stack) >>= \stk -> case stk of
  x : _ -> return x
  _     -> error "top: empty stack"

-- | False values are the number 0, and the empty array\/string\/block.
bool :: Val m -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk $ doBlock []]

unbool :: Bool -> Val m
unbool b = Int $ if b then 1 else 0

execute :: (Monad m) => Block m -> StateT (Golf m) m ()
execute blk = StateT $ runs (blk ^. blockDo) >=> \s -> return ((), s)

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

-- | Converts a string to a array of integers.
strToArr :: String -> [Val m]
strToArr = map $ Int . c2i

-- | Array to string conversion in cases like @[1 [2] \'xyz\']\'\'+@
arrToStr :: [Val m] -> String
arrToStr = concatMap $ \x -> case x of
  Int i -> [i2c i]
  Str s -> s
  Arr a -> arrToStr a
  Blk b -> b ^. blockStr

-- | Runs a command sequence, then pops a value off and evaluates it for truth.
predicate :: (Monad m) => Block m -> S m Bool
predicate b = execute b >> liftM (maybe False bool) spop

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

c2i :: Char -> Integer
c2i = fromIntegral . fromEnum

i2c :: Integer -> Char
i2c = toEnum . (.&. 0xFF) . fromIntegral

coerce :: (Monad m) => (Coerced m -> S m ()) -> S m ()
coerce f = binary $ \x y -> f $ case (x, y) of
  (Int a, Int b) -> Ints a b
  (Int _, Arr b) -> Arrs [x] b
  (Int a, Str b) -> Strs (show a) b
  (Int _, Blk b) -> Blks (doBlock [Push x]) b
  (Arr a, Int _) -> Arrs a [y]
  (Arr a, Arr b) -> Arrs a b
  (Arr a, Str b) -> Strs (arrToStr a) b
  (Arr _, Blk _) -> error "coerce: TODO implement arr->blk conversion"
  (Str a, Int b) -> Strs a (show b)
  (Str a, Arr b) -> Strs a (arrToStr b)
  (Str a, Str b) -> Strs a b
  (Str a, Blk b) -> Blks (strBlock a) b
  (Blk a, Int _) -> Blks a (doBlock [Push y])
  (Blk _, Arr _) -> error "coerce: TODO implement arr->blk conversion"
  (Blk a, Str b) -> Blks a (strBlock b)
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
lb = modify $ brackets ^: (0 :)

-- | @]@ ends an array \"literal\"
rb :: (Monad m) => S m ()
rb = gets (^. brackets) >>= \bts -> case bts of
  [] -> modify $ stack ^: \s -> [Arr $ reverse s]
  b : bs -> do
    modify $ brackets ^= bs   -- pop a bracket value off
    arr <- replicateM b spop' -- pop n elements off the stack
    spush $ Arr $ reverse arr -- reverse them and push on the new array

-- | @.@ duplicates the top value, by 1 pop and 2 pushes
dot :: (Monad m) => Val m -> S m ()
dot x = spush x >> spush x

-- | @~@ bitwise not (int), eval (blk\/str), push each (arr)
tilde :: (Monad m) => Val m -> S m ()
tilde x = case x of
  Int i -> spush $ Int $ complement i
  Arr a -> mapM_ spush a
  Blk b -> execute b
  Str s -> execute $ strBlock s

-- | @!@ boolean not: if in {@0@, @[]@, @\"\"@, @{}@}, push 1. else push 0.
bang :: (Monad m) => Val m -> S m ()
bang x = spush $ Int $ if bool x then 0 else 1

-- | @\@@ bring third value to top: @[z, y, x, ...]@ becomes @[x, z, y, ...]@
at :: (Monad m) => Val m -> Val m -> Val m -> S m ()
at x y z = spush y >> spush z >> spush x

-- | @\\@ swap top two elements: @[y, x, ...]@ becomes @[x, y, ...]@
backslash :: (Monad m) => Val m -> Val m -> S m ()
backslash x y = spush y >> spush x

-- | @;@ pop and discard top element
semicolon :: (Monad m) => S m ()
semicolon = spop >> return ()

-- | @,@ make @[0..n]@ (int), length (arr\/str), filter arr\/str by key (blk)
comma :: (Monad m) => Val m -> S m ()
comma x = case x of
  Int i -> spush $ Arr $ map Int [0 .. i-1]
  Arr a -> spush $ Int $ fromIntegral $ length a
  Str s -> spush $ Int $ fromIntegral $ length s
  Blk b -> spop >>= \mb -> case mb of -- take array a, then: filterBy b a
    Just (Int _) -> error "comma: can't execute '<int><blk>,'" -- .rb error
    Just (Arr a) -> blkFilter a >>= spush . Arr
    Just (Str s) -> blkFilter (strToArr s) >>= spush . Str . arrToStr
    Just (Blk b') -> blkFilter (strToArr $ b' ^. blockStr) >>=
      spush . Blk . strBlock . arrToStr
    Nothing -> spush x -- .rb error
    where blkFilter = filterM $ \v -> spush v >> predicate b

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
  Arr a -> spush $ Arr $ sort a -- in .rb, sorting different types is an error
  Str s -> spush $ Str $ sort s
  Blk b -> unary $ \y -> case y of
    Arr a -> sortOnM f a >>= spush . Arr
    Str s -> sortOnM (f . Int . c2i) s >>= spush . Str
    _ -> undefined
    where f z = spush z >> execute b >> spop'

sortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = mapM f xs >>= \ys ->
  return $ map fst $ sortBy (comparing snd) $ zip xs ys

-- | @+@ coerce: add (ints), concat (arrs\/strs\/blks)
plus :: (Monad m) => S m ()
plus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x + y
  Arrs x y -> spush $ Arr $ x ++ y
  Strs x y -> spush $ Str $ x ++ y
  Blks x y -> spush $ Blk $ Block
    { blockStr_ = (x ^. blockStr) ++ " " ++ (y ^. blockStr)
    , blockDo_ = (x ^. blockDo) ++ [Get " "] ++ (y ^. blockDo) }

-- | @|@ coerce: bitwise or (ints), setwise or (arrs\/strs\/blks)
pipe :: (Monad m) => S m ()
pipe = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .|. y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ strBlock $ op (x ^. blockStr) (y ^. blockStr)
  where op x y = nub $ union x y

-- | @|@ coerce: bitwise and (ints), setwise and (arrs\/strs\/blks)
ampersand :: (Monad m) => S m ()
ampersand = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .&. y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ strBlock $ op (x ^. blockStr) (y ^. blockStr)
  where op x y = nub $ intersect x y

-- | @^@ coerce: bitwise xor (ints), setwise xor (arrs\/strs\/blks)
caret :: (Monad m) => S m ()
caret = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ xor x y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ strBlock $ op (x ^. blockStr) (y ^. blockStr)
  where op x y = nub $ union x y \\ intersect x y

-- | @^@ coerce: subtract (ints), setwise difference (arrs\/strs\/blks)
minus :: (Monad m) => S m ()
minus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x - y
  Arrs x y -> spush $ Arr $ op x y
  Strs x y -> spush $ Str $ op x y
  Blks x y -> spush $ Blk $ strBlock $ op (x ^. blockStr) (y ^. blockStr)
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
  IntBlk x y -> replicateM_ (fromIntegral x) $ execute y
  -- join two sequences
  ArrArr _ _ -> error "star: TODO implement arr*arr join"
  ArrStr _ _ -> error "star: TODO implement arr*str join"
  StrStr x y -> spush $ Str $ intercalate y $ map (:"") x
  -- fold
  ArrBlk x y -> fold x y
  StrBlk x y -> fold (strToArr x) y
  -- ???
  BlkBlk _ y -> mapM_ (spush . Int . c2i) $ y ^. blockStr
    -- {anything}{abc}* ==> [97 98 99]~
    -- convert y to str, push each char int to stack
    -- probably a bug, but we'll copy it for now!
  where fold [] _ = return ()
        fold (x : xs) blk = do
          spush x
          forM_ xs $ \y -> spush y >> execute blk

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
  StrStr x y -> spush $ Arr $ map Str $ splitOn y x
  -- seq/blk: run block for each elem in seq
  ArrBlk x y -> forM_ x $ \v -> spush v >> execute y
  StrBlk x y -> forM_ (strToArr x) $ \v -> spush v >> execute y
  -- blk/blk: unfold
  BlkBlk cond body -> go >>= \xs -> semicolon >> spush (Arr xs) where
    go = unary dot >> predicate cond >>= \b ->
      if b then liftM2 (:) top $ execute body >> go else return []
  -- int/blk: error
  IntBlk _ _ -> error "slash: can't execute <int>/<blk>"

percent :: (Monad m) => S m ()
percent = order $ \o -> case o of
  -- int/int: modulo
  IntInt x y -> spush $ Int $ mod x y
  -- int/seq: select elems from y whose index mod x is 0
  IntArr x y -> if x < 0
    then spush $ Arr $ every (fromIntegral $ abs x) $ reverse y
    else spush $ Arr $ every (fromIntegral x) y
  IntStr x y -> if x < 0
    then spush $ Str $ every (fromIntegral $ abs x) $ reverse y
    else spush $ Str $ every (fromIntegral x) y
  -- seq/seq: split x on occurrences of y, but get rid of empty segments
  ArrArr x y -> spush $ Arr $ map Arr $ cleanSplitOn y x
  ArrStr x y -> spush $ Arr $ map Arr $ cleanSplitOn (strToArr y) x
  StrStr x y -> spush $ Arr $ map Str $ cleanSplitOn y x
  -- seq/blk: map elements
  ArrBlk x y -> lb >> forM_ x (\v -> spush v >> execute y) >> rb
  StrBlk x y -> lb >> forM_ (strToArr x) (\v -> spush v >> execute y) >> rb
  -- int/blk: error
  IntBlk _ _ -> error "percent: undefined operation int%blk"
  BlkBlk _ _ -> error "percent: undefined operation blk%blk"
  where every i xs = map head $ chunksOf i xs
        cleanSplitOn xs ys = filter (not . null) $ splitOn xs ys

less :: (Monad m) => S m ()
less = order $ \o -> case o of
  -- less than comparison
  IntInt x y -> spush $ unbool $ x < y
  ArrArr x y -> spush $ unbool $ x < y
  StrStr x y -> spush $ unbool $ x < y
  BlkBlk x y -> spush $ unbool $ x^.blockStr < y^.blockStr
  -- select elements in a sequence with index < n
  IntArr x y -> spush $ Arr $ index x y
  IntStr x y -> spush $ Str $ index x y
  IntBlk x y -> spush $ Blk $ strBlock $ index x $ y^.blockStr
  _ -> error "less: undefined '<' with two sequences of different types"
  where index n xs = if n < 0
          then genericTake (n + genericLength xs) xs
          else genericTake n xs

greater :: (Monad m) => S m ()
greater = order $ \o -> case o of
  -- greater than comparison
  IntInt x y -> spush $ unbool $ x > y
  ArrArr x y -> spush $ unbool $ x > y
  StrStr x y -> spush $ unbool $ x > y
  BlkBlk x y -> spush $ unbool $ x^.blockStr < y^.blockStr
  -- select elements in a sequence with index >= n
  IntArr x y -> spush $ Arr $ index x y
  IntStr x y -> spush $ Str $ index x y
  IntBlk x y -> spush $ Blk $ strBlock $ index x $ y^.blockStr
  _ -> error "greater: undefined '>' with two sequences of different types"
  where index n xs = if n < 0
          then genericDrop (n + genericLength xs) xs
          else genericDrop n xs

equal :: (Monad m) => S m ()
equal = order $ \o -> case o of
  -- For same types, test for equal.
  IntInt x y -> spush $ unbool $ x == y
  ArrArr x y -> spush $ unbool $ x == y
  StrStr x y -> spush $ unbool $ x == y
  BlkBlk x y -> spush $ unbool $ x^.blockStr == y^.blockStr
  ArrStr x y -> spush $ unbool $ x == strToArr y
  StrBlk x y -> spush $ unbool $ x == y^.blockStr
  -- For int and sequence, get at index.
  IntArr x y -> maybe (return ()) spush $ index x y
  IntStr x y -> maybe (return ()) (spush . Int . c2i) $ index x y
  IntBlk x y -> maybe (return ()) (spush . Int . c2i) $ index x $ y^.blockStr
  -- ???
  ArrBlk _ _ -> error "equal: undefined '=' with array and block"
  where index n xs = lookup n $ if n < 0
          then zip [-1, -2 ..] $ reverse xs
          else zip [0 ..] xs

question :: (Monad m) => S m ()
question = order $ \o -> case o of
  -- For two ints, exponent.
  IntInt x y -> spush $ Int $ x ^ y
  -- For int and seq, find element and push index.
  IntArr x y -> spush $ Int $ fromMaybe (-1) $ lookup (Int x) $ zip y [0..]
  IntStr x y -> spush $ Int $ fromMaybe (-1) $ lookup (i2c x) $ zip y [0..]
  -- For seq and blk, find element and push index.
  _ -> undefined -- TODO

primDo :: (Monad m) => S m ()
primDo = unary $ \x -> case x of
  Blk b -> go where go = predicate b >>= \p -> when p go
  _ -> error "primDo: 'do' expects block on top of stack"

primIf :: (Monad m) => S m ()
primIf = ternary $ \x y z -> case if bool x then y else z of
  Blk b -> execute b
  v     -> spush v

primAbs :: (Monad m) => S m ()
primAbs = unary $ \x -> case x of
  Int i -> spush $ Int $ abs i
  _     -> error $ "primAbs: 'abs' expected int arg, received: " ++ show x

primZip :: (Monad m) => S m ()
primZip = error "primZip: TODO implement zip"

primBase :: (Monad m) => S m ()
primBase = error "primBase: TODO implement base"

primWhile :: (Monad m) => S m ()
primWhile = binary f where
  f (Blk cond) (Blk body) = go where
    go = predicate cond >>= \b -> when b $ execute body >> go
  f _ _ = error "primWhile: 'while' expected 2 block arguments"

primUntil :: (Monad m) => S m ()
primUntil = binary f where
  f (Blk cond) (Blk body) = go where
    go = predicate cond >>= \b -> when (not b) $ execute body >> go
  f _ _ = error "primUntil: 'until' expected 2 block arguments"

primPrint :: S IO ()
primPrint = unary $ lift . putStr . output

primRand :: S IO ()
primRand = unary $ \x -> case x of
  Int i -> lift (getStdRandom $ randomR (0, i - 1)) >>= spush . Int
  _ -> error $ "primRand: 'rand' expected int argument, received: " ++ show x

--
-- And finally, the initial state with built-in functions
--

-- | A prelude consisting only of pure functions.
prelude :: (Monad m) => [(String, Val m)]
prelude =
  [ ("[", prim lb)
  , ("]", prim rb)
  , (".", prim $ unary dot)
  , ("~", prim $ unary tilde)
  , ("!", prim $ unary bang)
  , ("@", prim $ ternary at)
  , ("\\", prim $ binary backslash)
  , (";", prim semicolon)
  , (",", prim $ unary comma)
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
  , ("and", Blk $ strBlock "1$if")
  , ("or", Blk $ strBlock "1$\\if")
  , ("xor", Blk $ strBlock "\\!!{!}*")
  , ("n", Blk $ strBlock "\"\\n\"")
  , ("do", prim primDo)
  , ("if", prim primIf)
  , ("abs", prim primAbs)
  , ("zip", prim primZip)
  , ("base", prim primBase)
  , ("while", prim primWhile)
  , ("until", prim primUntil)
  ]

-- | A prelude consisting of pure functions, plus IO print functions.
preludeIO :: [(String, Val IO)]
preludeIO = prelude ++
  [ ("print", prim primPrint)
  , ("puts", Blk $ strBlock "print n print")
  , ("p", Blk $ strBlock "`puts")
  , ("rand", prim primRand)
  ]

-- | Returns an initial state with a certain prelude loaded.
emptyWith :: (Monad m) => [(String, Val m)] -> Golf m
emptyWith prel = variables ^= M.fromList prel $ empty
