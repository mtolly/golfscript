{-# LANGUAGE ViewPatterns #-}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad
import Data.Maybe (mapMaybe)
import Data.List (sort)

type S m = StateT (Golf m) m

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \s -> f s >>= \s' -> return ((), s')

bool :: Val m -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk []]

pop' :: (Monad m) => S m (Maybe (Val m))
pop' = gets pop >>= maybe (return Nothing) (\(x, g) -> put g >> return (Just x))

push' :: (Monad m) => Val m -> S m ()
push' x = modify (push x)

unary :: (Monad m) => (Val m -> S m ()) -> S m ()
unary f = pop' >>= maybe (return ()) f

prim :: (Monad m) => S m () -> Val m
prim s = Blk [Prim $ P $ execStateT s]

lb :: (Monad m) => S m ()
lb = modify $ \(Golf stk bts vrs) -> Golf stk (0 : bts) vrs

rb :: (Monad m) => S m ()
rb = modify $ \(Golf stk bts vrs) -> case bts of
  [] -> Golf [Arr $ reverse stk] [] vrs
  b : bs -> case splitAt b stk of
    (stkl, stkr) -> Golf ((Arr $ reverse stkl) : stkr) bs vrs

dot :: (Monad m) => S m ()
dot = unary $ \x -> push' x >> push' x

tilde :: (Monad m) => S m ()
tilde = unary $ \x -> case x of
  Int i -> push' $ Int $ complement i
  Arr a -> mapM_ push' a
  Blk b -> modifyM $ exec b
  Str s -> modifyM $ exec $ parse $ scan s

bang :: (Monad m) => S m ()
bang = unary $ \x -> push' $ Int $ if bool x then 0 else 1

at :: (Monad m) => S m ()
at = modify $ \g -> case g of
  Golf (x:y:z:xs) bts vrs -> Golf (z:x:y:xs) (map (max 3) bts) vrs
  -- "max 3" because it has to behave like we popped 3 and pushed 3.
  _ -> g

backslash :: (Monad m) => S m ()
backslash = modify $ \g -> case g of
  Golf (x:y:xs) bts vrs -> Golf (y:x:xs) (map (max 2) bts) vrs
  -- "max 2" because it has to behave like we popped 2 and pushed 2.
  _ -> g

semicolon :: (Monad m) => S m ()
semicolon = pop' >> return ()

strToArr :: String -> [Val m]
strToArr = map $ Int . fromEnum'

arrToStr :: [Val m] -> String
arrToStr = mapMaybe $ \x -> case x of
  Int i -> Just $ toEnum $ fromIntegral i
  _ -> Nothing

comma :: (Monad m) => S m ()
comma = unary $ \x -> case x of
  Int i -> push' $ Arr $ map Int [0 .. i-1]
  Arr a -> push' $ Int $ fromIntegral $ length a
  Str s -> push' $ Int $ fromIntegral $ length s
  Blk b -> unary $ \y -> case y of -- take array a, then: filterBy b a
    Arr a -> filterM (\v -> push' v >> predicate b) a >>= push' . Arr
    Str s -> filterM (\v -> push' v >> predicate b) (strToArr s) >>=
      push' . Str . arrToStr
    _ -> return ()

predicate :: (Monad m) => [Do m] -> S m Bool
predicate xs = modifyM (exec xs) >> liftM (maybe False bool) pop'

lp :: (Monad m) => S m ()
lp = unary $ \x -> case x of
  Int i -> push' $ Int $ i - 1
  Arr (v : vs) -> push' (Arr vs) >> push' v
  Str (c : cs) -> push' (Str cs) >> push' (Int $ fromEnum' c)
  _ -> push' x

rp :: (Monad m) => S m ()
rp = unary $ \x -> case x of
  Int i -> push' $ Int $ i + 1
  Arr (unsnoc -> Just (vs, v)) -> push' (Arr vs) >> push' v
  Str (unsnoc -> Just (cs, c)) -> push' (Str cs) >> push' (Int $ fromEnum' c)
  _ -> push' x

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

fromEnum' :: (Enum a, Integral b) => a -> b
fromEnum' = fromIntegral . fromEnum

backtick :: (Monad m) => S m ()
backtick = unary $ \x -> push' $ Str $ uneval [Push x]

dollar :: (Monad m) => S m ()
dollar = unary $ \x -> case x of
  Int i -> gets stack >>= \stk -> case lookup i (zip [0..] stk) of
    Nothing -> return ()
    Just v  -> push' v
  Arr a -> push' $ Arr $ sort a
  Str s -> push' $ Str $ sort s
  Blk _ -> undefined -- take a str/arr and sort by mapping

data Coerced m
  = Ints Integer Integer
  | Arrs [Val m] [Val m]
  | Strs String String
  | Blks [Do m] [Do m]
  deriving (Eq, Ord, Show, Read)

coerce :: (Monad m) => (Coerced m -> S m ()) -> S m ()
coerce = undefined

plus :: (Monad m) => S m ()
plus = coerce $ \c -> case c of
  Ints x y -> push' $ Int $ x + y
  Arrs x y -> push' $ Arr $ x ++ y
  Strs x y -> push' $ Str $ x ++ y
  Blks x y -> push' $ Blk $ x ++ [Get " "] ++ y

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
  ] }
