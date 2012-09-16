{
{-# OPTIONS_GHC -w #-}
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

\-? $digit+                       { Num . read }
$alphascore ($alphascore|$digit)+ { Var }
\" (\\ . | [^\\\"])* \"           { String . read }
\' (\\ . | [^\\\'])* \'           { String . rawString }

. { Var }

{

data Token
  = Var String
  | Num Integer
  | String String
  | LBrace
  | RBrace
  | Colon
  deriving (Eq, Ord, Show, Read)

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