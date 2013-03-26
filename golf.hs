module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment
import System.IO (hPutStrLn, stderr)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

main :: IO ()
main = getArgs >>= \argv -> case argv of
  fprog : _ -> getContents >>= \input -> readFile fprog >>= go input
  _         -> getContents >>= go ""

go :: String -> String -> IO ()
go input prog = do
  let p = parse $ scan prog
      s = (emptyWith preludeIO) { stack_ = [Str input] }
  result <- runErrorT $ evalStateT (runs p >> fmap output stackToArr) s
  case result of
    Left  err -> hPutStrLn stderr err
    Right str -> putStrLn str
