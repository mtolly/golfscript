module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment
import Data.Accessor

main :: IO ()
main = getArgs >>= \argv -> case argv of
  fprog : _ -> getContents >>= \input -> readFile fprog >>= go input
  _         -> getContents >>= go ""

go :: String -> String -> IO ()
go input prog = do
  g <- runs (parse $ scan prog) $ stack ^= [Str input] $ emptyWith ioPrelude
  putStrLn $ output $ stackToArr g
