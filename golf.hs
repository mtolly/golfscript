module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment
import Data.Functor.Identity

main = getArgs >>= \argv -> case argv of
  [prog] -> putStrLn $ uneval $ reverse $ map Push $ stack $
    runIdentity $ exec (parse $ scan prog) prelude
    -- todo: uneval isn't exactly right. top level strings shouldn't get quotes
  _ -> putStrLn "Usage: golf \"program\""
