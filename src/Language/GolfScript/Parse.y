{
module Language.GolfScript.Parse (scan, parse) where

import Language.GolfScript.Base
import Language.GolfScript.Scan
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  var { Var $$ }
  num { Num $$ }
  str { String $$ }
  '{' { LBrace }
  '}' { RBrace }
  ':' { Colon }

%%

DoList : Do DoList { $1 : $2 }
       | { [] }

Do : num { Push (Int $1) }
   | str { Push (Str $1) }
   | var { Get $1 }
   | ':' var { Set $2 }
   | '{' DoList '}' { Push (Blk $2) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
