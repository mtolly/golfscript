module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment

main = getArgs >>= \argv -> case argv of
  [prog] -> print $ exec (parse $ scan prog) prelude
  _ -> putStrLn "Usage: golf \"program\""
