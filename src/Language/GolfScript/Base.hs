{- | Core types and runtime. -}
module Language.GolfScript.Base where

import qualified Data.HashMap as M
import Control.Monad
import Data.List (intersperse)
import Data.Accessor
import Language.GolfScript.Scan
import Control.Applicative ((<|>))

-- | A value, parametrized by a monad for primitive functions.
data Val m
  = Int Integer
  | Arr [Val m]
  | Str String
  | Blk (Block m)
  deriving (Eq, Ord, Show, Read)

-- | A program command, parametrized by a monad for primitive functions.
data Do m
  = Push (Val m)  -- ^ Push a value onto the stack.
  | Get  String (Maybe (Val m))
  -- ^ Read a variable, with a possible default value.
  | Set  String   -- ^ Write to a variable.
  | Prim (Prim m) -- ^ A primitive built-in function.
  deriving (Eq, Ord, Show, Read)

-- | A block of code with both an executable and string representation.
data Block m = Block
  { blockDo_  :: [Do m]
  , blockStr_ :: String
  } deriving (Eq, Ord, Show, Read)

-- | The executable program expressed by a block.
blockDo :: Accessor (Block m) [Do m]
blockDo = accessor blockDo_ $ \x b -> b { blockDo_ = x }

-- | The string representation of a block.
blockStr :: Accessor (Block m) String
blockStr = accessor blockStr_ $ \x b -> b { blockStr_ = x }

-- | An opaque built-in monadic function. Because of the way the stack works, a
-- program can only execute Prim values; it can never handle them directly.
newtype Prim m = P { runP :: Golf m -> m (Golf m) }
instance Eq   (Prim m) where _ == _      = error "(==): can't equate Prim"
instance Ord  (Prim m) where compare _ _ = error "compare: can't compare Prim"
instance Show (Prim m) where show _      = "<prim>"
instance Read (Prim m) where readsPrec   = error "readsPrec: can't read Prim"

-- | The state of a GolfScript program.
data Golf m = Golf
  { stack_     :: [Val m]
  , brackets_  :: [Int]
  , variables_ :: M.Map String (Val m)
  } deriving (Eq, Ord, Show, Read)

-- | The program stack. Starts out containing a single string of standard input.
stack :: Accessor (Golf m) [Val m]
stack = accessor stack_ $ \x g -> g { stack_ = x }

-- | The way array brackets work is somewhat convoluted. The left bracket pushes
-- a zero onto this list. The 'push' command increments each of these numbers,
-- and the 'pop' command decrements them (while not letting them go below zero).
-- The right bracket pops a number off this list, 'take's that many values off
-- the stack, reverses them, and puts them into a new list.
brackets :: Accessor (Golf m) [Int]
brackets = accessor brackets_ $ \x g -> g { brackets_ = x }

-- | Named variables, stored in a hash table.
variables :: Accessor (Golf m) (M.Map String (Val m))
variables = accessor variables_ $ \x g -> g { variables_ = x }

-- | Creates a block by generating the string representation, given a program.
doBlock :: [Do m] -> Block m
doBlock dos = Block { blockDo_ = dos, blockStr_ = uneval dos }

-- | An initial state with an empty stack and no predefined variables.
empty :: Golf m
empty = Golf [] [] M.empty

-- | Push a value onto the stack.
push :: Val m -> Golf m -> Golf m
push x g = stack ^: (x :) $ brackets ^: map (+ 1) $ g

-- | Pop a value off the stack, or Nothing if the stack is empty.
pop :: Golf m -> Maybe (Val m, Golf m)
pop g = case g ^. stack of
  [] -> Nothing
  (x : xs) -> Just (x, stack ^= xs $ brackets ^: map sub1 $ g)
    where sub1 n = max 0 $ n - 1

-- | Run a single command.
run :: (Monad m) => Do m -> Golf m -> m (Golf m)
run d g = case d of
  Get v def -> case (M.lookup v $ g ^. variables) <|> def of
    Nothing -> return g
    Just (Blk b) -> runs (b ^. blockDo) g -- execute block
    Just x       -> return $ push x g     -- push x onto stack
  Set v -> return $ case pop g of
    Just (x, _) -> variables ^: M.insert v x $ g
    Nothing -> g
  Prim (P f) -> f g
  Push x -> return $ push x g

-- | Run a list of commands in sequence.
runs :: (Monad m) => [Do m] -> Golf m -> m (Golf m)
runs = foldr (>=>) return . map run

unscan :: [Token] -> String
unscan = concatMap $ \t -> case t of
  Var x -> x
  IntLit (_, s) -> s
  StrLit s -> show s
  LBrace -> "{"
  RBrace -> "}"
  Colon -> ":"

unparse :: [Do m] -> [Token]
unparse = concatMap $ \d -> case d of
  Get x _ -> [Var x]
  Set x -> [Colon, Var x]
  Prim _ -> error "unparse: can't unparse Prim"
  Push v -> case v of
    Int i -> [IntLit (i, show i)]
    Arr a -> [Var "["] ++ inner ++ [Var "]"]
      where inner = unparse $ intersperse (Get " " Nothing) (map Push a)
    Str s -> [StrLit s]
    Blk b -> [LBrace] ++ unparse (b ^. blockDo) ++ [RBrace]

--- | Produces a program which executes a series of actions, with two conditions:
--- the array bracket operators aren't overwritten, and the space character
--- hasn't been assigned a value.
uneval :: [Do m] -> String
uneval = unscan . unparse

-- | The function used to print the stack's contents on program end.
-- Equivalent to the @to_gs@ method from the original interpreter.
output :: Val m -> String
output (Int i) = show i
output (Arr a) = concatMap output a
output (Str s) = s
output (Blk b) = "{" ++ (b ^. blockStr) ++ "}"

stackToArr :: Golf m -> Val m
stackToArr = Arr . reverse . getVal stack
