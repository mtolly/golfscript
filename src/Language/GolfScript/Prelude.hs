{-# LANGUAGE ViewPatterns #-}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad (void)

type S = State Golf

pop' :: S (Maybe Val)
pop' = gets pop >>= maybe (return Nothing)
  (\(x, g) -> put g >> return (Just x))

push' :: Val -> S ()
push' x = modify (push x)

unary :: (Val -> S ()) -> S ()
unary f = pop' >>= maybe (return ()) f

prim :: S () -> Val
prim s = Blk [Prim $ P $ execState s]

lb :: S ()
lb = modify $ \(Golf stk bts vrs) -> Golf stk (0 : bts) vrs

rb :: S ()
rb = modify $ \(Golf stk bts vrs) -> case bts of
  [] -> Golf [Arr $ reverse stk] [] vrs
  b : bs -> case splitAt b stk of
    (stkl, stkr) -> Golf ((Arr $ reverse stkl) : stkr) bs vrs

dot :: S ()
dot = unary $ \x -> push' x >> push' x

tilde :: S ()
tilde = unary $ \x -> case x of
  Int i -> push' $ Int $ complement i
  Arr a -> mapM_ push' a
  Blk b -> modify $ exec b
  Str s -> modify $ exec $ parse $ scan s

bang :: S ()
bang = unary $ \x -> push' $ Int $
  if elem x [Int 0, Arr [], Str "", Blk []] then 1 else 0

at :: S ()
at = modify $ \g -> case g of
  Golf (x:y:z:xs) bts vrs -> Golf (z:x:y:xs) bts vrs
  _ -> g

backslash :: S ()
backslash = modify $ \g -> case g of
  Golf (x:y:xs) bts vrs -> Golf (y:x:xs) bts vrs
  _ -> g

semicolon :: S ()
semicolon = void pop'

comma :: S ()
comma = unary $ \x -> case x of
  Int i -> push' $ Arr $ map Int [0 .. i-1]
  Arr a -> push' $ Int $ fromIntegral $ length a
  Str s -> push' $ Int $ fromIntegral $ length s
  Blk _ -> undefined -- take array a, then: filterBy b a

lp :: S ()
lp = unary $ \x -> case x of
  Int i -> push' $ Int $ i - 1
  Arr (v : vs) -> push' (Arr vs) >> push' v
  _ -> push' x

rp :: S ()
rp = unary $ \x -> case x of
  Int i -> push' $ Int $ i + 1
  Arr (unsnoc -> Just (vs, v)) -> push' (Arr vs) >> push' v
  _ -> push' x

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  y : ys -> Just (reverse ys, y)
  _ -> Nothing

prelude :: Golf
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
  ] }
