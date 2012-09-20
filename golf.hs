module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import Data.Functor.Identity

main :: IO ()
main = interact $ \x ->
  (output . stackToArr . runIdentity $ runs (parse $ scan x) prelude) ++ "\n"
