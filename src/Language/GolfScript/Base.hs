module Language.GolfScript.Base where

import qualified Data.HashMap as M

data Val
  = Int Integer
  | Arr [Val]
  | Str String
  | Blk [Do]
  deriving (Eq, Ord, Show, Read)

data Do
  = Push Val
  | Get String
  | Set String
  | Prim Prim
  deriving (Eq, Ord, Show, Read)

newtype Prim = P (Golf -> Golf)
instance Eq   Prim where _ == _ = True
instance Ord  Prim where compare _ _ = EQ
instance Show Prim where show _ = "<prim>"
instance Read Prim where readsPrec = error "readsPrec: can't read Prim"

data Golf = Golf
  { stack :: [Val]
  , brackets :: [Int] -- nums of elements to 'take' off stack at right bracket
  , vars :: M.Map String Val
  } deriving (Eq, Show, Read)

empty :: Golf
empty = Golf [] [] M.empty

push :: Val -> Golf -> Golf
push x (Golf stk bts vs) = Golf (x : stk) (map (+ 1) bts) vs

pop :: Golf -> Maybe (Val, Golf)
pop (Golf [] _ _) = Nothing
pop (Golf (x : xs) bts vrs) = Just (x, Golf xs (map sub1 bts) vrs)
  where sub1 n = max 0 $ n - 1

act :: Do -> Golf -> Golf
act d g@(Golf _ _ vrs) = case d of
  Get v -> case M.lookup v vrs of
    Nothing -> g -- undefined variable, no effect
    Just (Blk b) -> exec b g -- execute block
    Just x -> push x g -- push x onto stack
  Set v -> case pop g of
    Just (x, Golf stk bts _) -> Golf stk bts $ M.insert v x vrs
    Nothing -> g
  Prim (P f) -> f g
  Push x -> push x g

exec :: [Do] -> Golf -> Golf
exec = foldr (flip (.)) id . map act

uneval :: [Do] -> String
uneval = concatMap $ \d -> case d of
  Get x -> x
  Set x -> ':' : x
  Prim _ -> "prim" -- should never happen
  Push v -> case v of
    Int i -> show i
    Arr a -> "[" ++ uneval (map Push a) ++ "]"
    Str s -> show s
    Blk b -> "{" ++ uneval b ++ "}"
