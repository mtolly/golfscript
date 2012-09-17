module Language.GolfScript.Base where

import qualified Data.HashMap as M
import Control.Monad
import Data.List (intersperse)

-- | A value, parametrized by a monad for primitive functions.
data Val m
  = Int Integer
  | Arr [Val m]
  | Str String
  | Blk [Do m]
  deriving (Eq, Ord, Show, Read)

-- | A program command, parametrized by a monad for primitive functions.
data Do m
  = Push (Val m) -- ^ Push a value onto the stack.
  | Get String -- ^ Read a variable.
  | Set String -- ^ Write to a variable.
  | Prim (Prim m) -- ^ A primitive built-in function.
  deriving (Eq, Ord, Show, Read)

-- | An opaque built-in monadic function. Because of the way the stack works, a
-- program can only execute Prim values; it can never handle them directly.
newtype Prim m = P { runP :: (Golf m -> m (Golf m)) }
instance Eq   (Prim m) where _ == _ = True
instance Ord  (Prim m) where compare _ _ = EQ
instance Show (Prim m) where show _ = "<prim>"
instance Read (Prim m) where readsPrec = error "readsPrec: can't read Prim"

-- | The state of a GolfScript program.
data Golf m = Golf
  { stack :: [Val m]
  -- ^ The program stack. Starts out containing a single string of standard
  -- input.
  , brackets :: [Int]
  -- ^ The way array brackets work is somewhat convoluted. The left bracket
  -- pushes a zero onto this list. The 'push' command increments each of these
  -- numbers, and the 'pop' command decrements them (while not letting them go
  -- below zero). The right bracket pops a number off this list, 'take's that
  -- many values off the stack, reverses them, and puts them into a new list.
  , vars :: M.Map String (Val m)
  -- ^ Named variables, stored in a hash table.
  } deriving (Eq, Ord, Show, Read)

-- | An initial state with an empty stack and no predefined variables.
empty :: Golf m
empty = Golf [] [] M.empty

-- | Push a value onto the stack.
push :: Val m -> Golf m -> Golf m
push x (Golf stk bts vrs) = Golf (x : stk) (map (+ 1) bts) vrs

-- | Pop a value off the stack, or Nothing if the stack is empty.
pop :: Golf m -> Maybe (Val m, Golf m)
pop (Golf [] _ _) = Nothing
pop (Golf (x : xs) bts vrs) = Just (x, Golf xs (map sub1 bts) vrs)
  where sub1 n = max 0 $ n - 1

-- | Run a single command.
run :: (Monad m) => Do m -> Golf m -> m (Golf m)
run d g@(Golf _ _ vrs) = case d of
  Get v -> case M.lookup v vrs of
    Nothing -> return g -- undefined variable, no effect
    Just (Blk b) -> runs b g -- execute block
    Just x -> return $ push x g -- push x onto stack
  Set v -> return $ case pop g of
    Just (x, Golf stk bts _) -> Golf stk bts $ M.insert v x vrs
    Nothing -> g
  Prim (P f) -> f g
  Push x -> return $ push x g

-- | Run a list of commands in sequence.
runs :: (Monad m) => [Do m] -> Golf m -> m (Golf m)
runs = foldr (>=>) return . map run

-- | Produces a program which executes a series of actions, with two conditions:
-- the array bracket operators aren't overwritten, and the space character
-- hasn't been assigned a value.
uneval :: [Do m] -> String
uneval = concatMap go . intersperse (Get " ") where
  -- todo: avoid putting unnecessary space chars in?
  go d = case d of
    Get x -> x
    Set x -> ':' : x
    Prim _ -> error "uneval: can't print primitive function"
    Push v -> case v of
      Int i -> show i
      Arr a -> "[" ++ uneval (map Push a) ++ "]"
      Str s -> show s
      Blk b -> "{" ++ uneval b ++ "}"
