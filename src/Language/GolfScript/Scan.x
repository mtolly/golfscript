{
{-# OPTIONS_GHC -w #-}
{- | Generated scanner for GolfScript programs -}
module Language.GolfScript.Scan (scan, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alphascore = [a-zA-Z_]

tokens :-

\# [^\n\r]+ ;

\: { const Colon }
\{ { const LBrace }
\} { const RBrace }

\-? $digit+                       { IntLit . read }
$alphascore ($alphascore|$digit)+ { Var }
\" (\\ . | [^\\\"])* \"           { StrLit . read }
\' (\\ . | [^\\\'])* \'           { StrLit . rawString }

. { Var }

{

data Token
  = Var String
  | IntLit Integer
  | StrLit String
  | LBrace
  | RBrace
  | Colon
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
