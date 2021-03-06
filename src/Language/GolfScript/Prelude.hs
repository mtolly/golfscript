{- | The standard set of built-in functions included with GolfScript. -}
module Language.GolfScript.Prelude
( prelude, preludeIO, preludeWrappedIO
, emptyWith
, WrappedIO, runWrappedIO
) where

import Control.Arrow (second)
import Control.Monad
  ( unless, when, liftM, forM, forM_, replicateM_, filterM, replicateM
  , liftM2, liftM3
  )
import Data.Bits ((.&.), (.|.), xor, complement)
import Data.Foldable (mapM_)
import Data.List
  ( transpose, unfoldr, tails, isPrefixOf, findIndex, elemIndex
  , genericTake, genericDrop, genericLength, intersect, intercalate
  , genericReplicate, union, (\\), nub, sort, sortBy
  )
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Prelude hiding (mapM_)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (runWriterT, tell, WriterT)
import qualified Data.HashMap as M
import Data.List.Split (splitOn, chunksOf)
import System.Random (randomR, getStdRandom)

import Language.GolfScript.Base
import Language.GolfScript.Parse

-- | Two values popped off the stack, coerced to the same type. For @Foos x y@,
-- @y@ was on top of @x@ in the original stack.
data Coerced m
  = Ints Integer Integer
  | Arrs [Val m] [Val m]
  | Strs String String
  | Blks (Block m) (Block m)
  deriving (Eq, Ord, Show, Read)

-- | Two values popped off the stack, placed in a standard type-priority order.
-- For @FooFoo x y@, @y@ was on top of @x@ in the original stack.
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

-- | Packages a state function as a value that can be assigned to a variable.
prim :: (Monad m) => Golf m () -> Val m
prim s = Blk $ doBlock [Prim $ P s]

--
-- Helper functions
--

-- | Returns the top value of the stack, without doing a pop operation.
-- Bracket boundaries aren't changed.
top :: (Monad m) => Golf m (Val m)
top = stack >>= \s -> case s of
  x : _ -> return x
  _     -> crash "top: empty stack"

-- | False values are the number 0, and the empty array\/string\/block.
bool :: Val m -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk $ doBlock []]

unbool :: Bool -> Val m
unbool b = Int $ if b then 1 else 0

execute :: (Monad m) => Block m -> Golf m ()
execute = runs . blockDo_

pop2 :: (Monad m) => Golf m (Val m, Val m)
pop2 = liftM2 (flip (,)) pop pop

pop3 :: (Monad m) => Golf m (Val m, Val m, Val m)
pop3 = liftM3 (\z y x -> (x, y, z)) pop pop pop

anyToArr :: Val m -> [Val m]
anyToArr x = case x of
  Int _ -> [x] -- .rb behavior is a complete bug, so this is just filler
  Arr a -> a
  Str s -> strToArr s
  Blk b -> strToArr $ blockStr_ b

anyToStr :: Val m -> String
anyToStr x = case x of
  Int i -> show i -- again, this is meaningless
  Arr a -> arrToStr a
  Str s -> s
  Blk b -> blockStr_ b

anyToBlk :: Val m -> Block m
anyToBlk x = case x of
  Int i -> strBlock $ show i -- uselesssssss
  Arr a -> strBlock $ arrToStr a
  Str s -> strBlock s
  Blk b -> b

-- | Converts a string to a array of integers.
strToArr :: String -> [Val m]
strToArr = map $ Int . c2i

-- | Array to string conversion in cases like @[1 [2] \'xyz\']\'\'+@
arrToStr :: [Val m] -> String
arrToStr = concatMap $ \x -> case x of
  Int i -> [i2c i]
  Str s -> s
  Arr a -> arrToStr a
  Blk b -> blockStr_ b

-- | Runs a command sequence, then pops a value off and evaluates it for truth.
predicate :: (Monad m) => Block m -> Golf m Bool
predicate b = execute b >> liftM (maybe False bool) popMaybe

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

c2i :: Char -> Integer
c2i = fromIntegral . fromEnum

i2c :: Integer -> Char
i2c = toEnum . (.&. 0xFF) . fromIntegral

coerce' :: Val m -> Val m -> Coerced m
coerce' x y = case (x, y) of
  (Int a, Int b) -> Ints a b
  (Int _, Arr b) -> Arrs [x] b
  (Int a, Str b) -> Strs (show a) b
  (Int _, Blk b) -> Blks (doBlock [Push x]) b
  (Arr a, Int _) -> Arrs a [y]
  (Arr a, Arr b) -> Arrs a b
  (Arr a, Str b) -> Strs (arrToStr a) b
  (Arr a, Blk b) -> Blks (strBlock $ output $ star' a " ") b
  -- [(self*Gstring.new(' ')).to_s.compile,b]
  (Str a, Int b) -> Strs a (show b)
  (Str a, Arr b) -> Strs a (arrToStr b)
  (Str a, Str b) -> Strs a b
  (Str a, Blk b) -> Blks (strBlock a) b
  (Blk a, Int _) -> Blks a (doBlock [Push y])
  (Blk a, Arr b) -> Blks a (strBlock $ output $ star' b " ")
  (Blk a, Str b) -> Blks a (strBlock b)
  (Blk a, Blk b) -> Blks a b
  where star' :: [Val m] -> String -> Val m
        star' a b = case a of
          [] -> Str ""
          r:rs -> foldl (\v i -> (v +! Str b) +! i) (r `coerceTo` Str b) rs
        a +! b = plus' $ coerce' a b

coerce :: (Monad m) => Golf m (Coerced m)
coerce = pop2 >>= \(x, y) -> return $ coerce' x y

coerceTo :: Val m -> Val m -> Val m
x `coerceTo` y = case coerce' x y of
  Ints x' _ -> Int x'
  Arrs x' _ -> Arr x'
  Strs x' _ -> Str x'
  Blks x' _ -> Blk x'

order :: (Monad m) => Golf m (Ordered m)
order = pop2 >>= \(x, y) -> return $ case (x, y) of
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
lb :: (Monad m) => Golf m ()
lb = brackets >>= setBrackets . (0 :)

-- | @]@ ends an array \"literal\"
rb :: (Monad m) => Golf m ()
rb = brackets >>= \bts -> case bts of
  [] -> stackToArr >>= setStack . (:[])
  b : bs -> do
    setBrackets bs           -- pop a bracket value off
    arr <- replicateM b pop  -- pop n elements off the stack
    push $ Arr $ reverse arr -- reverse them and push on the new array

-- | @.@ duplicates the top value, by 1 pop and 2 pushes
dot :: (Monad m) => Golf m ()
dot = pop >>= \x -> push x >> push x

-- | @~@ bitwise not (int), eval (blk\/str), push each (arr)
tilde :: (Monad m) => Golf m ()
tilde = pop >>= \x -> case x of
  Int i -> push $ Int $ complement i
  Arr a -> mapM_ push a
  Blk b -> execute b
  Str s -> execute $ strBlock s

-- | @!@ boolean not: if in {@0@, @[]@, @\"\"@, @{}@}, push 1. else push 0.
bang :: (Monad m) => Golf m ()
bang = pop >>= \x -> push $ Int $ if bool x then 0 else 1

-- | @\@@ bring third value to top: @[z, y, x, ...]@ becomes @[x, z, y, ...]@
at :: (Monad m) => Golf m ()
at = pop3 >>= \(x, y, z) -> push y >> push z >> push x

-- | @\\@ swap top two elements: @[y, x, ...]@ becomes @[x, y, ...]@
backslash :: (Monad m) => Golf m ()
backslash = pop2 >>= \(x, y) -> push y >> push x

-- | @;@ pop and discard top element
semicolon :: (Monad m) => Golf m ()
semicolon = popMaybe >> return ()

-- | @,@ make @[0..n-1]@ (int), length (arr\/str), filter arr\/str by key (blk)
comma :: (Monad m) => Golf m ()
comma = pop >>= \x -> case x of
  Int i -> push $ Arr $ map Int [0 .. i-1]
  Arr a -> push $ Int $ fromIntegral $ length a
  Str s -> push $ Int $ fromIntegral $ length s
  Blk b -> pop >>= \y -> case y of -- take array a, then: filterBy b a
    Int _ -> crash "comma: undefined operation '<int><blk>,'"
    Arr a -> blkFilter a >>= push . Arr
    Str s -> blkFilter (strToArr s) >>= push . Str . arrToStr
    Blk b' -> blkFilter (strToArr $ blockStr_ b') >>=
      push . Blk . strBlock . arrToStr
    where blkFilter = filterM $ \v -> push v >> predicate b

-- | @(@ decrement (int), uncons from left (arr\/str)
lp :: (Monad m) => Golf m ()
lp = pop >>= \x -> case x of
  Int i -> push $ Int $ i - 1
  Arr (v : vs) -> push (Arr vs) >> push v
  Str (c : cs) -> push (Str cs) >> push (Int $ c2i c)
  Blk (Block {blockStr_ = c : cs}) -> do
    push $ Blk $ strBlock cs
    push $ Int $ c2i c
  _ -> crash "'(' tried to left-uncons empty arr/str/blk"

-- | @)@ increment (int), uncons from right (arr\/str)
rp :: (Monad m) => Golf m ()
rp = pop >>= \x -> case x of
  Int i -> push $ Int $ i + 1
  Arr a -> case unsnoc a of
    Nothing      -> crash "')' tried to right-uncons empty array"
    Just (vs, v) -> push (Arr vs) >> push v
  Str s -> case unsnoc s of
    Nothing      -> crash "')' tried to right-uncons empty string"
    Just (cs, c) -> push (Str cs) >> push (Int $ c2i c)
  Blk b -> case unsnoc $ blockStr_ b of
    Nothing      -> crash "')' tried to right-uncons empty block"
    Just (cs, c) -> push (Blk $ strBlock cs) >> push (Int $ c2i c)

-- | @`@ uneval: convert a value to the code which generates that value
backtick :: (Monad m) => Golf m ()
backtick = pop >>= \x -> push $ Str $ uneval [Push x]
  -- Note: "Push (Int i)" is not ok because "i" might be a variable. But we're
  -- just unevaling it immediately, so it just gets converted to a string
  -- anyway. If it is then evaled, it will be expanded into the correct
  -- "Get (show i) (Just (Int i))".

-- | @$@ copy nth item from stack (int), sort (arr\/str), take str\/arr and
-- sort by mapping (blk)
dollar :: (Monad m) => Golf m ()
dollar = pop >>= \x -> case x of
  Int i -> stack >>= \stk -> mapM_ push $ lookup i $ if i >= 0
    then zip [0..] stk
    else zip [-1,-2..] $ reverse stk
  Arr a -> push $ Arr $ sort a -- in .rb, sorting different types is an error
  Str s -> push $ Str $ sort s
  Blk b -> pop >>= \y -> case y of
    Arr a -> sortOnM f a >>= push . Arr
    Str s -> sortOnM (f . Int . c2i) s >>= push . Str
    Blk b' -> sortOnM (f . Int . c2i) (blockStr_ b') >>= push . Blk . strBlock
    Int _ -> crash "'$' tried to sort an integer via a block"
    where f z = push z >> execute b >> pop

sortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = mapM f xs >>= \ys ->
  return $ map fst $ sortBy (comparing snd) $ zip xs ys

plus' :: Coerced m -> Val m
plus' c = case c of
  Ints x y -> Int $ x + y
  Arrs x y -> Arr $ x ++ y
  Strs x y -> Str $ x ++ y
  Blks x y -> Blk $ Block
    { blockStr_ = blockStr_ x ++ " " ++ blockStr_ y
    , blockDo_  = blockDo_ x ++ [Get " " Nothing] ++ blockDo_ y }

-- | @+@ coerce: add (ints), concat (arrs\/strs\/blks)
plus :: (Monad m) => Golf m ()
plus = coerce >>= push . plus'

pipe' :: Coerced m -> Val m
pipe' c = case c of
  Ints x y -> Int $ x .|. y
  Arrs x y -> Arr $ op x y
  Strs x y -> Str $ op x y
  Blks x y -> Blk $ strBlock $ op (blockStr_ x) (blockStr_ y)
  where op x y = nub $ union x y

-- | @|@ coerce: bitwise or (ints), setwise or (arrs\/strs\/blks)
pipe :: (Monad m) => Golf m ()
pipe = coerce >>= push . pipe'

ampersand' :: Coerced m -> Val m
ampersand' c = case c of
  Ints x y -> Int $ x .&. y
  Arrs x y -> Arr $ op x y
  Strs x y -> Str $ op x y
  Blks x y -> Blk $ strBlock $ op (blockStr_ x) (blockStr_ y)
  where op x y = nub $ intersect x y

-- | @|@ coerce: bitwise and (ints), setwise and (arrs\/strs\/blks)
ampersand :: (Monad m) => Golf m ()
ampersand = coerce >>= push . ampersand'

caret' :: Coerced m -> Val m
caret' c = case c of
  Ints x y -> Int $ xor x y
  Arrs x y -> Arr $ op x y
  Strs x y -> Str $ op x y
  Blks x y -> Blk $ strBlock $ op (blockStr_ x) (blockStr_ y)
  where op x y = nub $ union x y \\ intersect x y

-- | @^@ coerce: bitwise xor (ints), setwise xor (arrs\/strs\/blks)
caret :: (Monad m) => Golf m ()
caret = coerce >>= push . caret'

minus' :: Coerced m -> Val m
minus' c = case c of
  Ints x y -> Int $ x - y
  Arrs x y -> Arr $ op x y
  Strs x y -> Str $ op x y
  Blks x y -> Blk $ strBlock $ op (blockStr_ x) (blockStr_ y)
  where op x y = filter (`notElem` y) x

-- | @^@ coerce: subtract (ints), setwise difference (arrs\/strs\/blks)
minus :: (Monad m) => Golf m ()
minus = coerce >>= push . minus'

-- | @*@ order: multiply (int*int), run n times (int*blk), multiply and join
-- (int*seq), join with separator (seq*seq), fold (seq*blk)
star :: (Monad m) => Golf m ()
star = order >>= \o -> case o of
  -- multiply
  IntInt x y -> push $ Int $ x * y
  -- concat n copies of seq
  IntArr x y -> push $ Arr $ concat $ genericReplicate x y
  IntStr x y -> push $ Str $ concat $ genericReplicate x y
  -- run a block n times
  IntBlk x y -> replicateM_ (fromIntegral x) $ execute y
  -- join two sequences
  ArrArr x y -> push $ case x of
    [] -> Arr []
    r:rs -> foldl (\v i -> (v +! Arr y) +! i) (r `coerceTo` Arr y) rs
  ArrStr x y -> push $ case x of
    [] -> Str ""
    r:rs -> foldl (\v i -> (v +! Str y) +! i) (r `coerceTo` Str y) rs
  StrStr x y -> push $ Str $ intercalate y $ map (:"") x
  -- fold
  ArrBlk x y -> fold x y
  StrBlk x y -> fold (strToArr x) y
  BlkBlk x y -> fold (strToArr $ blockStr_ y) x
  where fold [] _ = return ()
        fold (x : xs) blk = do
          push x
          forM_ xs $ \y -> push y >> execute blk
        x +! y = plus' $ coerce' x y

slash :: (Monad m) => Golf m ()
slash = order >>= \o -> case o of
  -- int/int: divide
  IntInt x y -> push $ Int $ div x y
  -- int/seq: split seq into chunks of n elements
  -- if int is negative, reverse before chunking
  IntArr x y -> push $ Arr $ map Arr $ if x < 0
    then chunksOf (fromIntegral $ abs x) (reverse y)
    else chunksOf (fromIntegral x) y
  IntStr x y -> push $ Arr $ map Str $ if x < 0
    then chunksOf (fromIntegral $ abs x) (reverse y)
    else chunksOf (fromIntegral x) y
  -- seq/seq: split x on occurrences of y
  ArrArr x y -> push $ Arr $ map Arr $ splitOn y x
  ArrStr x y -> push $ Arr $ map Arr $ splitOn (strToArr y) x
  StrStr x y -> push $ Arr $ map Str $ splitOn y x
  -- seq/blk: run block for each elem in seq
  ArrBlk x y -> forM_ x $ \v -> push v >> execute y
  StrBlk x y -> forM_ (strToArr x) $ \v -> push v >> execute y
  -- blk/blk: unfold
  BlkBlk cond body -> go >>= \xs -> semicolon >> push (Arr xs) where
    go = dot >> predicate cond >>= \b ->
      if b then liftM2 (:) top $ execute body >> go else return []
  -- int/blk: error
  IntBlk _ _ -> crash "slash: undefined operation '<int><blk>/'"

percent' :: (Monad m) => Ordered m -> Golf m (Val m)
percent' o = case o of
  -- int/int: modulo
  IntInt x y -> return $ Int $ mod x y
  -- int/seq: select elems from y whose index mod x is 0
  IntArr x y -> return $ Arr $ if x < 0
    then every (fromIntegral $ abs x) $ reverse y
    else every (fromIntegral x) y
  IntStr x y -> return $ Str $ if x < 0
    then every (fromIntegral $ abs x) $ reverse y
    else every (fromIntegral x) y
  -- seq/seq: split x on occurrences of y, but get rid of empty segments
  ArrArr x y -> return $ Arr $ map Arr $ cleanSplitOn y x
  ArrStr x y -> return $ Arr $ map Arr $ cleanSplitOn (strToArr y) x
  StrStr x y -> return $ Arr $ map Str $ cleanSplitOn y x
  -- seq/blk: map elements
  ArrBlk x y -> liftM Arr $ mapArr y x
  StrBlk x y -> liftM (Str . arrToStr) $ mapArr y $ strToArr x
  -- int/blk: error
  IntBlk _ _ -> crash "percent: undefined operation '<int><blk>%'"
  BlkBlk _ _ -> crash "percent: undefined operation '<blk><blk>%'"
  where every i xs = map head $ chunksOf i xs
        cleanSplitOn xs ys = filter (not . null) $ splitOn xs ys
        mapArr blk arr = liftM concat $ forM arr $ \v -> do
          len <- liftM length stack
          push v
          execute blk
          len2 <- liftM length stack
          (tk, dp) <- liftM (splitAt $ len2 - len) stack
          setStack dp
          return $ reverse tk

percent :: (Monad m) => Golf m ()
percent = order >>= percent' >>= push

less :: (Monad m) => Golf m ()
less = order >>= \o -> case o of
  -- less than comparison
  IntInt x y -> push $ unbool $ x < y
  ArrArr x y -> push $ unbool $ x < y
  StrStr x y -> push $ unbool $ x < y
  BlkBlk x y -> push $ unbool $ blockStr_ x < blockStr_ y
  -- select elements in a sequence with index < n
  IntArr x y -> push $ Arr $ index x y
  IntStr x y -> push $ Str $ index x y
  IntBlk x y -> push $ Blk $ strBlock $ index x $ blockStr_ y
  _ -> crash "less: undefined '<' with two sequences of different types"
  where index n xs = if n < 0
          then genericTake (n + genericLength xs) xs
          else genericTake n xs

greater :: (Monad m) => Golf m ()
greater = order >>= \o -> case o of
  -- greater than comparison
  IntInt x y -> push $ unbool $ x > y
  ArrArr x y -> push $ unbool $ x > y
  StrStr x y -> push $ unbool $ x > y
  BlkBlk x y -> push $ unbool $ blockStr_ x < blockStr_ y
  -- select elements in a sequence with index >= n
  IntArr x y -> push $ Arr $ index x y
  IntStr x y -> push $ Str $ index x y
  IntBlk x y -> push $ Blk $ strBlock $ index x $ blockStr_ y
  _ -> crash "greater: undefined '>' with two sequences of different types"
  where index n xs = if n < 0
          then genericDrop (n + genericLength xs) xs
          else genericDrop n xs

equal :: (Monad m) => Golf m ()
equal = order >>= \o -> case o of
  -- Test for equality.
  IntInt x y -> push $ unbool $ x == y
  ArrArr x y -> push $ unbool $ x == y
  StrStr x y -> push $ unbool $ x == y
  BlkBlk x y -> push $ unbool $ blockStr_ x == blockStr_ y
  ArrStr x y -> push $ unbool $ x == strToArr y
  StrBlk x y -> push $ unbool $ x == blockStr_ y
  ArrBlk x y -> push $ unbool $ x == strToArr (blockStr_ y)
  -- Get element at index, or nothing.
  IntArr x y -> maybe (return ()) push $ index x y
  IntStr x y -> maybe (return ()) (push . Int . c2i) $ index x y
  IntBlk x y -> maybe (return ()) (push . Int . c2i) $ index x $ blockStr_ y
  where index n xs = lookup n $ if n < 0
          then zip [-1, -2 ..] $ reverse xs
          else zip [0 ..] xs

question :: (Monad m) => Golf m ()
question = order >>= \o -> case o of
  -- Exponent.
  IntInt x y -> push $ Int $ x ^ y
  -- Find element, and push index or -1.
  IntArr x y -> indexOf (Int x) y
  IntStr x y -> indexOf (i2c x) y
  ArrArr x y -> indexOf (Arr y) x
  ArrStr x y -> indexOf (Str y) x
  -- Find substring, and push index or -1.
  StrStr x y -> push $ Int $ fromIntegral $ fromMaybe (-1) $ y `infixOf` x
  -- Find element, and push element or nothing.
  ArrBlk x y -> findBy y x
  StrBlk x y -> findBy y $ strToArr x
  BlkBlk x y -> findBy x $ strToArr $ blockStr_ y
  -- ???
  IntBlk _ _ -> crash "question: undefined operation <int><blk>?"
  where findBy _   []     = return ()
        findBy blk (x:xs) = push x >> predicate blk >>= \b ->
          if b then push x else findBy blk xs
        indexOf x xs =
          push $ Int $ fromIntegral $ fromMaybe (-1) $ elemIndex x xs

infixOf :: (Eq a) => [a] -> [a] -> Maybe Int
xs `infixOf` ys = findIndex (xs `isPrefixOf`) $ tails ys

primDo :: (Monad m) => Golf m ()
primDo = pop >>= \x -> case x of
  Blk b -> go where go = predicate' b >>= \p -> when p go
  _ -> crash "primDo: 'do' expects block on top of stack"

-- | Like predicate, but dynamically calls @!@ and then checks if that leaves 0.
predicate' :: (Monad m) => Block m -> Golf m Bool
predicate' b = execute b >> dynamicBool

dynamicBool :: (Monad m) => Golf m Bool
dynamicBool = run (Get "!" Nothing) >> liftM (== Int 0) pop

primIf :: (Monad m) => Golf m ()
primIf = do
  (t, f) <- pop2
  b <- dynamicBool
  case if b then t else f of
    Blk blk -> execute blk
    v       -> push v

primAbs :: (Monad m) => Golf m ()
primAbs = pop >>= \x -> case x of
  Int i -> push $ Int $ abs i
  _     -> crash $ "primAbs: 'abs' expected int arg, received: " ++ show x

primZip' :: (Monad m) => [Val m] -> Golf m [Val m]
primZip' [] = return []
primZip' (x:xs) = case x of
  Int _ -> crash "primZip: int found in an array passed to 'zip'"
  Arr a -> return $ map Arr $ transpose $ a : map anyToArr xs
  Str s -> return $ map Str $ transpose $ s : map anyToStr xs
  Blk b -> return $
    map (Blk . strBlock) $ transpose $ map blockStr_ $ b : map anyToBlk xs

-- | Only well-defined for an array of arrs/strs/blks (can be heterogeneous).
primZip :: (Monad m) => Golf m ()
primZip = pop >>= \x -> case x of
  Arr a -> primZip' a >>= push . Arr
  _     -> crash $ "primZip: 'zip' expected array, received: " ++ show x

primBase :: (Monad m) => Golf m ()
primBase = pop2 >>= \(x, y) -> case (x, y) of
  (Int n, Int r) -> push $ Arr $ map Int $ reverse $ unfoldr getDigit $ abs n
    where getDigit 0 = Nothing
          getDigit i = case divMod i r of (d, m) -> Just (m, d)
  (Arr dgts, Int r) -> dgts' >>= push . Int . sum . zipWith (*) places
    where dgts' = liftM reverse $ mapM getInt dgts
          getInt (Int i) = return i
          getInt _       = crash "primBase: non-Int digit"
          places = iterate (* r) 1
  _ -> crash "primBase: invalid args, expected <int><int>base or <arr><int>base"

primWhile :: (Monad m) => Golf m ()
primWhile = pop2 >>= uncurry f where
  f (Blk cond) (Blk body) = go where
    go = predicate' cond >>= \b -> when b $ execute body >> go
  f _ _ = crash "primWhile: 'while' expected 2 block arguments"

primUntil :: (Monad m) => Golf m ()
primUntil = pop2 >>= uncurry f where
  f (Blk cond) (Blk body) = go where
    go = predicate' cond >>= \b -> unless b $ execute body >> go
  f _ _ = crash "primUntil: 'until' expected 2 block arguments"

primPrint :: Golf IO ()
primPrint = pop >>= liftIO . putStr . output

primRand :: (MonadIO m) => Golf m ()
primRand = pop >>= \x -> case x of
  Int i -> liftIO (getStdRandom $ randomR (0, i - 1)) >>= push . Int
  _ -> crash $ "primRand: 'rand' expected int argument, received: " ++ show x

--
-- And finally, the initial state with built-in functions
--

-- | A prelude consisting only of pure functions.
prelude :: (Monad m) => [(String, Val m)]
prelude =
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
  , ("and"  , Blk $ strBlock "1$if")
  , ("or"   , Blk $ strBlock "1$\\if")
  , ("xor"  , Blk $ strBlock "\\!!{!}*")
  , ("n"    , Blk $ strBlock "\"\\n\"")
  , ("do"   , prim primDo)
  , ("if"   , prim primIf)
  , ("abs"  , prim primAbs)
  , ("zip"  , prim primZip)
  , ("base" , prim primBase)
  , ("while", prim primWhile)
  , ("until", prim primUntil)
  ]

-- | A prelude consisting of pure functions, plus IO print functions.
preludeIO :: [(String, Val IO)]
preludeIO = prelude ++
  [ ("print", prim primPrint)
  , ("puts" , Blk $ strBlock "print n print")
  , ("p"    , Blk $ strBlock "`puts")
  , ("rand" , prim primRand)
  ]

-- | Returns an initial state with a certain prelude loaded.
emptyWith :: (Monad m) => [(String, Val m)] -> GolfState m
emptyWith prel = empty { variables_ = M.fromList prel }

-- | A wrapper around IO where output can be directed to a string, for testing
-- purposes.
type WrappedIO = WriterT [String] IO

runWrappedIO :: WrappedIO a -> IO (a, String)
runWrappedIO = fmap (second concat) . runWriterT

primPrintWrapped :: Golf WrappedIO ()
primPrintWrapped = pop >>= lift . lift . tell . (:[]) . output

preludeWrappedIO :: [(String, Val WrappedIO)]
preludeWrappedIO = prelude ++
  [ ("print", prim primPrintWrapped)
  , ("puts" , Blk $ strBlock "print n print")
  , ("p"    , Blk $ strBlock "`puts")
  , ("rand" , prim primRand)
  ]
