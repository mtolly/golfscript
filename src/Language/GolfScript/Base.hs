{- | Core types and runtime. -}
module Language.GolfScript.Base
( Val(..)
, Do(..)
, Block(..)
, Prim(..)
, GolfState(..), Golf
, stack, setStack, brackets, setBrackets, variables, setVariables
, doBlock
, empty
, push, pop, popMaybe
, run, runs
, unscan, unparse, uneval
, output
, stackToArr
, crash
) where

import qualified Data.HashMap as M
import Control.Monad
import Data.List (intersperse)
import Language.GolfScript.Scan
import Control.Applicative ((<|>))
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class

-- | A value, parametrized by a monad for primitive functions.
data Val m
  = Int Integer
  | Arr [Val m]
  | Str String
  | Blk (Block m)
  deriving (Eq, Ord, Show, Read)

-- | A program command, parametrized by a monad for primitive functions.
data Do m
  = Push (Val m)
  -- ^ Push a value onto the stack.
  | Get  String (Maybe (Val m))
  -- ^ Read a variable, with a possible default value.
  | Set  String
  -- ^ Write to a variable.
  | Prim (Prim m)
  -- ^ A primitive built-in function.
  deriving (Eq, Ord, Show, Read)

-- | A block of code with both an executable and string representation.
data Block m = Block
  { blockDo_  :: [Do m]
  , blockStr_ :: String
  } deriving (Eq, Ord, Show, Read)

-- | An opaque built-in monadic function. Because of the way the stack works, a
-- program can only execute Prim values; it can never handle them directly.
newtype Prim m = P { runP :: Golf m () }
instance Eq   (Prim m) where _ == _      = error "(==): can't equate Prim"
instance Ord  (Prim m) where compare _ _ = error "compare: can't compare Prim"
instance Show (Prim m) where show _      = "<prim>"
instance Read (Prim m) where readsPrec   = error "readsPrec: can't read Prim"

-- | The state of a GolfScript program.
data GolfState m = GolfState
  { stack_     :: [Val m]
  , brackets_  :: [Int]
  , variables_ :: M.Map String (Val m)
  }

type Golf m = StateT (GolfState m) (ErrorT String m)

crash :: (Monad m) => String -> Golf m a
crash = lift . throwError

-- | The program stack. Starts out containing a single string of standard input.
stack :: (Monad m) => Golf m [Val m]
stack = gets stack_

setStack :: (Monad m) => [Val m] -> Golf m ()
setStack s = modify $ \gs -> gs { stack_ = s }

-- | The way array brackets work is somewhat convoluted. The left bracket pushes
-- a zero onto this list. The 'push' command increments each of these numbers,
-- and the 'pop' command decrements them (while not letting them go below zero).
-- The right bracket pops a number off this list, 'take's that many values off
-- the stack, reverses them, and puts them into a new list.
brackets :: (Monad m) => Golf m [Int]
brackets = gets brackets_

setBrackets :: (Monad m) => [Int] -> Golf m ()
setBrackets bs = modify $ \gs -> gs { brackets_ = bs }

-- | Named variables, stored in a hash table.
variables :: (Monad m) => Golf m (M.Map String (Val m))
variables = gets variables_

setVariables :: (Monad m) => (M.Map String (Val m)) -> Golf m ()
setVariables vs = modify $ \gs -> gs { variables_ = vs }

-- | Creates a block by generating the string representation, given a program.
doBlock :: [Do m] -> Block m
doBlock dos = Block { blockDo_ = dos, blockStr_ = uneval dos }

-- | An initial state with an empty stack and no predefined variables.
empty :: GolfState m
empty = GolfState
  { stack_      = []
  , brackets_   = []
  , variables_  = M.empty
  }

-- | Push a value onto the stack.
push :: (Monad m) => Val m -> Golf m ()
push x = do
  stack >>= setStack . (x :)
  brackets >>= setBrackets . map (+ 1)

popMaybe :: (Monad m) => Golf m (Maybe (Val m))
popMaybe = stack >>= \s -> case s of
  [] -> return Nothing
  x : xs -> let sub1 n = max 0 $ n - 1 in do
    setStack xs
    brackets >>= setBrackets . map sub1
    return $ Just x

-- | Pop a value off the stack, or Nothing if the stack is empty.
pop :: (Monad m) => Golf m (Val m)
pop = popMaybe >>= maybe (crash "pop: empty stack") return

-- | Run a single command.
run :: (Monad m) => Do m -> Golf m ()
run d = case d of
  Get v def -> variables >>= \vs -> case M.lookup v vs <|> def of
    Nothing      -> return ()
    Just (Blk b) -> runs (blockDo_ b) -- execute block
    Just x       -> push x            -- push non-block onto stack
  Set v -> pop >>= \x -> variables >>= setVariables . M.insert v x
    -- todo: what if stack is empty? crash or nothing?
  Prim (P f) -> f
  Push x -> push x

-- | Run a list of commands in sequence.
runs :: (Monad m) => [Do m] -> Golf m ()
runs = mapM_ run

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
    Blk b -> [LBrace] ++ unparse (blockDo_ b) ++ [RBrace]

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
output (Blk b) = "{" ++ (blockStr_ b) ++ "}"

stackToArr :: (Monad m) => Golf m (Val m)
stackToArr = liftM (Arr . reverse) stack
