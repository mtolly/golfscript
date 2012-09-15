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
\" (\\ . | [^\\\"])* \"                       { String . read }

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

}