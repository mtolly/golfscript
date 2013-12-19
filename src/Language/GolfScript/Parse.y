{
{- | Generated parser for GolfScript programs -}
module Language.GolfScript.Parse
( eval
, strBlock
) where

import Data.List (intersperse)
import Data.Maybe (mapMaybe)

import Language.GolfScript.Base
import Language.GolfScript.Scan
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  var { Var    $$ }
  int { IntLit $$ }
  str { StrLit $$ }
  '{' { LBrace $$ }
  '}' { RBrace $$ }
  ':' { Colon  $$ }

%%

TopList : Do TopList { $1 ++ $2 }
        | ':' str TopList { $3 }
        | ':' '{' TopList { $3 }
        | ':' '}' TopList { $3 }
        | ':' { [] }
        | { [] }

DoList : Do DoList { $1 ++ $2 }
       | ':' str DoList { $3 }
       | ':' '{' DoList { $3 }
       | ':' '}' DoList { $3 }
       | { [] }

Do : int { let (p, i, s) = $1 in [Posn $ Just p, Get s $ Just $ Int i] }
   | str { let (p, s) = $1 in [Posn $ Just p, Push $ Str s] }
   | var { let (p, s) = $1 in [Posn $ Just p, Get s Nothing] }
   | ':' var { let p = $1; (_, s) = $2 in [Posn $ Just p, Set s] }
   | ':' int { let p = $1; (_, _, s) = $2 in [Posn $ Just p, Set s] }
   | '{' DoList '}' { let p = $1 in [Posn $ Just p, Push $ Blk $ doBlock $2] }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- | Lexes and parses a program in one step.
eval :: String -> [Do m]
eval = parse . scan

noPosition :: [Do m] -> [Do m]
noPosition = mapMaybe $ \x -> case x of
  Posn _ -> Nothing
  Push (Blk (Block d s)) -> Just $ Push $ Blk $ Block (noPosition d) s
  Get v (Just (Blk (Block d s))) -> Just $ Get v $ Just $ Blk $ Block (noPosition d) s
  _ -> Just x

-- | Creates a block by parsing a string of code.
strBlock :: String -> Block m
strBlock str = Block { blockDo_ = Posn Nothing : noPosition (eval str), blockStr_ = str }

}
