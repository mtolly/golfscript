module Language.GolfScript.Base where

import qualified Data.HashMap as M
import Control.Monad

data Val m
  = Int Integer
  | Arr [Val m]
  | Str String
  | Blk [Do m]
  deriving (Eq, Ord, Show, Read)

data Do m
  = Push (Val m)
  | Get String
  | Set String
  | Prim (Prim m)
  deriving (Eq, Ord, Show, Read)

newtype Prim m = P (Golf m -> m (Golf m))
instance Eq   (Prim m) where _ == _ = True
instance Ord  (Prim m) where compare _ _ = EQ
instance Show (Prim m) where show _ = "<prim>"
instance Read (Prim m) where readsPrec = error "readsPrec: can't read Prim"

data Golf m = Golf
  { stack :: [Val m]
  , brackets :: [Int] -- nums of elements to 'take' off stack at right bracket
  , vars :: M.Map String (Val m)
  } deriving (Eq, Show, Read)

empty :: Golf m
empty = Golf [] [] M.empty

push :: Val m -> Golf m -> Golf m
push x (Golf stk bts vs) = Golf (x : stk) (map (+ 1) bts) vs

pop :: Golf m -> Maybe (Val m, Golf m)
pop (Golf [] _ _) = Nothing
pop (Golf (x : xs) bts vrs) = Just (x, Golf xs (map sub1 bts) vrs)
  where sub1 n = max 0 $ n - 1

act :: (Monad m) => Do m -> Golf m -> m (Golf m)
act d g@(Golf _ _ vrs) = case d of
  Get v -> case M.lookup v vrs of
    Nothing -> return g -- undefined variable, no effect
    Just (Blk b) -> exec b g -- execute block
    Just x -> return $ push x g -- push x onto stack
  Set v -> return $ case pop g of
    Just (x, Golf stk bts _) -> Golf stk bts $ M.insert v x vrs
    Nothing -> g
  Prim (P f) -> f g
  Push x -> return $ push x g

exec :: (Monad m) => [Do m] -> Golf m -> m (Golf m)
exec = foldr (>=>) return . map act

uneval :: [Do m] -> String
uneval = concatMap $ \d -> case d of
  Get x -> x
  Set x -> ':' : x
  Prim _ -> "prim" -- should never happen
  Push v -> case v of
    Int i -> show i
    Arr a -> "[" ++ uneval (map Push a) ++ "]"
    Str s -> show s
    Blk b -> "{" ++ uneval b ++ "}"
