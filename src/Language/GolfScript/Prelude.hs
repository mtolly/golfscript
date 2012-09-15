{-# LANGUAGE ViewPatterns #-}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits

pushEach :: [Val] -> Golf -> Golf
pushEach vs g = foldl (flip push) g vs

unary :: (Val -> Golf -> Golf) -> Prim
unary f = P $ \g -> case pop g of
  Nothing -> g
  Just (x, g') -> f x g'

prim :: Prim -> Val
prim p = Blk [Prim p]

lb :: Prim
lb = P $ \(Golf stk bts vrs) -> Golf stk (0 : bts) vrs

rb :: Prim
rb = P $ \(Golf stk bts vrs) -> case bts of
  [] -> Golf [Arr $ reverse stk] [] vrs
  b : bs -> case splitAt b stk of
    (stkl, stkr) -> Golf ((Arr $ reverse stkl) : stkr) bs vrs

dot :: Prim
dot = unary $ \x g -> push x $ push x g

tilde :: Prim
tilde = unary $ \x g -> case x of
  Int i -> push (Int $ complement i) g
  Arr a -> pushEach a g
  Blk b -> exec b g
  Str s -> exec (parse $ scan s) g

bang :: Prim
bang = unary $ \x -> push $ Int $
  if elem x [Int 0, Arr [], Str "", Blk []] then 1 else 0

at :: Prim
at = P $ \g -> case g of
  Golf (x:y:z:xs) bts vrs -> Golf (z:x:y:xs) bts vrs
  _ -> g

backslash :: Prim
backslash = P $ \g -> case g of
  Golf (x:y:xs) bts vrs -> Golf (y:x:xs) bts vrs
  _ -> g

semicolon :: Prim
semicolon = unary $ \_ g -> g

comma :: Prim
comma = unary $ \x g -> case x of
  Int i -> push (Arr $ map Int [0 .. i-1]) g
  Arr a -> push (Int $ fromIntegral $ length a) g
  Str s -> push (Int $ fromIntegral $ length s) g
  Blk _ -> undefined -- take array a, then: filterBy b a

lp :: Prim
lp = unary $ \x g -> case x of
  Int i -> push (Int $ i - 1) g
  Arr (v : vs) -> push v $ push (Arr vs) g
  _ -> g

rp :: Prim
rp = unary $ \x g -> case x of
  Int i -> push (Int $ i + 1) g
  Arr (unsnoc -> Just (vs, v)) -> push v $ push (Arr vs) g
  _ -> g

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
