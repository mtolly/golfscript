{
{- | Generated parser for GolfScript programs -}
module Language.GolfScript.Parse
( scan, parse, eval
, Token(..)
, strBlock
) where

import Language.GolfScript.Base
import Language.GolfScript.Scan
import Data.List (intersperse)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  var { Var $$ }
  int { IntLit $$ }
  str { StrLit $$ }
  '{' { LBrace }
  '}' { RBrace }
  ':' { Colon }

%%

TopList : Do TopList { $1 : $2 }
        | ':' str TopList { $3 }
        | ':' '{' TopList { $3 }
        | ':' '}' TopList { $3 }
        | ':' { [] }
        | { [] }

DoList : Do DoList { $1 : $2 }
       | ':' str DoList { $3 }
       | ':' '{' DoList { $3 }
       | ':' '}' DoList { $3 }
       | { [] }

Do : int { case $1 of (i, s) -> Get s (Just $ Int i) }
   | str { Push (Str $1) }
   | var { Get $1 Nothing }
   | ':' var { Set $2 }
   | ':' int { Set (snd $2) }
   | '{' DoList '}' { Push (Blk $ doBlock $2) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

eval :: String -> [Do m]
eval = parse . scan

-- | Creates a block by generating a program, given a string representation.
strBlock :: String -> Block m
strBlock str = Block { blockDo_ = eval str, blockStr_ = str }

}
