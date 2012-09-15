{-# LANGUAGE ViewPatterns #-}
module Language.GolfScript.Prelude where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import qualified Data.HashMap as M
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad (void, filterM)
import Data.Maybe (mapMaybe)

type S = State Golf

bool :: Val -> Bool
bool x = notElem x [Int 0, Arr [], Str "", Blk []]

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
bang = unary $ \x -> push' $ Int $ if bool x then 0 else 1

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

strToArr :: String -> [Val]
strToArr = map $ Int . fromIntegral . fromEnum

arrToStr :: [Val] -> String
arrToStr = mapMaybe $ \x -> case x of
  Int i -> Just $ toEnum $ fromIntegral $ i
  _ -> Nothing

comma :: S ()
comma = unary $ \x -> case x of
  Int i -> push' $ Arr $ map Int [0 .. i-1]
  Arr a -> push' $ Int $ fromIntegral $ length a
  Str s -> push' $ Int $ fromIntegral $ length s
  Blk b -> unary $ \y -> case y of -- take array a, then: filterBy b a
    Arr a -> filterM (\v -> push' v >> predicate b) a >>= push' . Arr
    Str s -> filterM (\v -> push' v >> predicate b) (strToArr s) >>=
      push' . Str . arrToStr
    _ -> return ()

predicate :: [Do] -> S Bool
predicate xs = modify (exec xs) >> fmap (maybe False bool) pop'

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

backtick :: S ()
backtick = unary $ \x -> push' $ Str $ uneval [Push x]

-- | Represents two values popped off the stack in type priority order.
-- For two values of equal type, "FooFoo x y" means stack is [x, y, ..]
data TwoVals
  = IntInt Integer Integer
  | IntArr Integer [Val]
  | IntStr Integer String
  | IntBlk Integer [Do]
  | ArrArr [Val] [Val]
  | ArrStr [Val] String
  | ArrBlk [Val] [Do]
  | StrStr String String
  | StrBlk String [Do]
  | BlkBlk [Do] [Do]
  deriving (Eq, Ord, Show, Read)

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
  , ("`", prim backtick)
  ] }
