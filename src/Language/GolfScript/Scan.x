{
{-# OPTIONS_GHC -w #-}
{- | Generated scanner for GolfScript programs -}
module Language.GolfScript.Scan
( scan
, Token(..)
) where

import Prelude hiding (lex)
}

%wrapper "posn"

$digit = 0-9
$alphascore = [a-zA-Z_]

tokens :-

\# [^\n\r]* ;

\: { lex $ \p _ -> Colon p }
\{ { lex $ \p _ -> LBrace p }
\} { lex $ \p _ -> RBrace p }

\-? $digit+                       { lex $ \p s -> IntLit (p, read s, s) }
$alphascore ($alphascore|$digit)+ { lex $ \p s -> Var (p, s) }
\" (\\ . | ([^\\\"] | \n))* \"    { lex $ \p s -> StrLit (p, read s) }
\' (\\ . | ([^\\\'] | \n))* \'    { lex $ \p s -> StrLit (p, rawString s) }

(. | \n) { lex $ \p s -> Var (p, s) }

{

lex :: ((Int, Int) -> String -> Token) -> AlexPosn -> String -> Token
lex f (AlexPn _ l c) s = f (l, c) s

-- | Scanned token type for GolfScript code.
data Token
  = Var ((Int, Int), String)
  -- ^ A variable read/write, except for integer literals.
  | IntLit ((Int, Int), Integer, String)
  -- ^ Integer literals can also be variables. If so, the variable name is the
  -- exact lexed token, which is the `String` in the pair.
  | StrLit ((Int, Int), String)
  | LBrace (Int, Int)
  | RBrace (Int, Int)
  | Colon (Int, Int)
  deriving (Eq, Ord, Show, Read)

-- | As it turns out, this function will never raise an error, because any
-- character by itself can be lexed as a variable read.
scan :: String -> [Token]
scan = alexScanTokens

-- | Reads a single-quoted string, where only backslash and single-quote can
-- be escaped.
rawString :: String -> String
rawString ('\'' : s) = go s where
  go ('\\' : '\\' : xs) = '\\' : go xs
  go ('\\' : '\'' : xs) = '\'' : go xs
  go ('\'' : _) = ""
  go (x : xs) = x : go xs
  go "" = ""
rawString _ = error "rawString: doesn't start with single quote"

}
