module Main where

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import System.Environment
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [fprog] | fprog /= "-?" -> getContents >>= \c -> readFile fprog >>= go c
  []                      -> getContents >>= go ""
  _                       -> printUsage

go :: String -> String -> IO ()
go input prog = do
  let p = eval prog
      s = emptyWith preludeIO
  result <- runGolf (push (Str input) >> runs p >> fmap output stackToArr) s
  case result of
    Left  err -> hPutStrLn stderr err
    Right str -> putStrLn str

printUsage :: IO ()
printUsage = getProgName >>= \pn -> mapM_ (hPutStrLn stderr)
  [ "GolfScript interpreter in Haskell by Michael Tolly <onyxite@gmail.com>"
  , "Usage: "++pn++" prog.gs < stdin.txt (stdin to program)"
  , "       "++pn++" < prog.gs           (empty stdin)"
  , "       "++pn++" -?                  (print usage)"
  ]
