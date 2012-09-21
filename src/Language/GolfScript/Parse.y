{
{- | Generated parser for GolfScript programs -}
module Language.GolfScript.Parse (scan, parse, Token(..)) where

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

Top : Do Top { $1 : $2 }
    | ':' { [] }
    | '{' Top { [Push (Blk $2)] } -- allows an unterminated block
    | { [] }

DoList : Do DoList { $1 : $2 }
       | ':' { [] } -- allows a (meaningless) colon at EOF
       | { [] }

Do : int { Push (Int $1) }
   | str { Push (Str $1) }
   | var { Get $1 }
   | ':' var { Set $2 }
   | '{' DoList '}' { Push (Blk $2) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
