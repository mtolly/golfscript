module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment
import Data.Functor.Identity
import Data.Accessor

main = getArgs >>= \argv -> case argv of
  [prog] -> putStrLn $ output $ stackToArr $
    runIdentity $ runs (parse $ scan prog) prelude
  _ -> putStrLn "Usage: golf \"program\""
