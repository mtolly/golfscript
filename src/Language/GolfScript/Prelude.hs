{-# LANGUAGE ViewPatterns #-}
{- | The standard set of built-in functions included with GolfScript. -}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad
import Data.Maybe (mapMaybe)
import Data.List

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

spop :: (Monad m) => S m (Maybe (Val m))
spop = gets pop >>= maybe (return Nothing) (\(x, g) -> put g >> return (Just x))

-- | False values are the number 0, and the empty array/string/block.
bool :: Val m -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk []]

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
strToArr = map $ Int . fromEnum'

arrToStr :: [Val m] -> String
arrToStr = mapMaybe $ \x -> case x of
  Int i -> Just $ toEnum' i
  _ -> Nothing

-- | Runs a command sequence, then pops a value off and evaluates it for truth.
predicate :: (Monad m) => [Do m] -> S m Bool
predicate xs = modifyM (runs xs) >> liftM (maybe False bool) spop

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Integral a, Enum b) => a -> b
toEnum' = toEnum . fromIntegral

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
lb = modify $ \(Golf stk bts vrs) -> Golf stk (0 : bts) vrs

-- | @]@ ends an array \"literal\"
rb :: (Monad m) => S m ()
rb = modify $ \(Golf stk bts vrs) -> case bts of
  [] -> Golf [Arr $ reverse stk] [] vrs
  b : bs -> case splitAt b stk of
    (stkl, stkr) -> Golf ((Arr $ reverse stkl) : stkr) bs vrs

-- | @.@ duplicates the top value, by 1 pop and 2 pushes
dot :: (Monad m) => S m ()
dot = unary $ \x -> spush x >> spush x

-- | @~@ bitwise not (int), eval (blk/str), push each (arr)
tilde :: (Monad m) => S m ()
tilde = unary $ \x -> case x of
  Int i -> spush $ Int $ complement i
  Arr a -> mapM_ spush a
  Blk b -> modifyM $ runs b
  Str s -> modifyM $ runs $ parse $ scan s

-- | @!@ boolean not: if in 0,[],"",{}, push 1. else push 0.
bang :: (Monad m) => S m ()
bang = unary $ \x -> spush $ Int $ if bool x then 0 else 1

-- | @\@@ bring third value to top: @[x,y,z,...]@ becomes @[z,x,y,...@
at :: (Monad m) => S m ()
at = ternary $ \x y z -> spush y >> spush z >> spush x

-- | @\\@ swap top two elements: @[x,y,...]@ becomes @[y,x,...]@
backslash :: (Monad m) => S m ()
backslash = binary $ \x y -> spush y >> spush x

-- | @;@ pop and discard top element
semicolon :: (Monad m) => S m ()
semicolon = spop >> return ()

comma :: (Monad m) => S m ()
comma = unary $ \x -> case x of
  Int i -> spush $ Arr $ map Int [0 .. i-1]
  Arr a -> spush $ Int $ fromIntegral $ length a
  Str s -> spush $ Int $ fromIntegral $ length s
  Blk b -> spop >>= \mb -> case mb of -- take array a, then: filterBy b a
    Just (Arr a) -> filterM (\v -> spush v >> predicate b) a >>= spush . Arr
    Just (Str s) -> filterM (\v -> spush v >> predicate b) (strToArr s) >>=
      spush . Str . arrToStr
    Just (Int _) -> undefined -- TODO: filter single int?
    Just (Blk _) -> undefined -- TODO: ???
    Nothing -> spush x -- push the block back on

lp :: (Monad m) => S m ()
lp = unary $ \x -> case x of
  Int i -> spush $ Int $ i - 1
  Arr (v : vs) -> spush (Arr vs) >> spush v
  Str (c : cs) -> spush (Str cs) >> spush (Int $ fromEnum' c)
  _ -> spush x

rp :: (Monad m) => S m ()
rp = unary $ \x -> case x of
  Int i -> spush $ Int $ i + 1
  Arr (unsnoc -> Just (vs, v)) -> spush (Arr vs) >> spush v
  Str (unsnoc -> Just (cs, c)) -> spush (Str cs) >> spush (Int $ fromEnum' c)
  _ -> spush x

backtick :: (Monad m) => S m ()
backtick = unary $ \x -> spush $ Str $ uneval [Push x]

dollar :: (Monad m) => S m ()
dollar = unary $ \x -> case x of
  Int i -> gets stack >>= \stk -> case lookup i (zip [0..] stk) of
    Nothing -> return ()
    Just v  -> spush v
  Arr a -> spush $ Arr $ sort a
  Str s -> spush $ Str $ sort s
  Blk _ -> undefined -- TODO: take a str/arr and sort by mapping

plus :: (Monad m) => S m ()
plus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x + y
  Arrs x y -> spush $ Arr $ x ++ y
  Strs x y -> spush $ Str $ x ++ y
  Blks x y -> spush $ Blk $ x ++ [Get " "] ++ y

pipe :: (Monad m) => S m ()
pipe = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .|. y
  Arrs x y -> spush $ Arr $ union x y
  Strs x y -> spush $ Str $ union x y
  Blks _ _ -> undefined -- TODO: ???

ampersand :: (Monad m) => S m ()
ampersand = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x .&. y
  Arrs x y -> spush $ Arr $ intersect x y
  Strs x y -> spush $ Str $ intersect x y
  Blks _ _ -> undefined -- TODO: ???

caret :: (Monad m) => S m ()
caret = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ xor x y
  Arrs x y -> spush $ Arr $ union x y \\ intersect x y
  Strs x y -> spush $ Str $ union x y \\ intersect x y
  Blks _ _ -> undefined -- TODO: ???

minus :: (Monad m) => S m ()
minus = coerce $ \c -> case c of
  Ints x y -> spush $ Int $ x - y
  Arrs x y -> spush $ Arr $ x \\ y
  Strs x y -> spush $ Str $ x \\ y
  Blks _ _ -> undefined -- TODO: ???

--
-- And finally, the initial state with built-in functions
--

prelude :: (Monad m) => Golf m
prelude = empty { vars = M.fromList
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
  ] }
