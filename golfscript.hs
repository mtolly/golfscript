module Main where

import Data.Version (showVersion)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout, Handle)

import Language.GolfScript.Base
import Language.GolfScript.Parse
import Language.GolfScript.Prelude
import Paths_golfscript (version)

main :: IO ()
main = getArgs >>= \argv -> case argv of
  ["-v"]  -> printVersion stdout
  ["-?"]  -> printUsage stdout
  [fprog] -> getContents >>= \c -> readFile fprog >>= go c
  []      -> getContents >>= go ""
  _       -> do
    printVersion stderr
    printUsage stderr
    exitFailure

go :: String -> String -> IO ()
go input prog = do
  let p = eval prog
      s = emptyWith preludeIO
  result <- runGolf (push (Str input) >> runs p >> fmap output stackToArr) s
  case result of
    Left  err -> hPutStrLn stderr err
    Right str -> putStrLn str

printVersion :: Handle -> IO ()
printVersion h = getProgName >>= \pn -> mapM_ (hPutStrLn h)
  [ pn ++ " " ++ showVersion version
  , "GolfScript language by Darren Smith < http://www.golfscript.com/golfscript >"
  , "Interpreter in Haskell by Michael Tolly < miketolly@gmail.com >"
  ]

printUsage :: Handle -> IO ()
printUsage h = getProgName >>= \pn -> mapM_ (hPutStrLn h)
  [ "Usage: "++pn++" prog.gs < stdin.txt # program with stdin"
  , "       "++pn++" < prog.gs           # empty stdin"
  , "       "++pn++" -?                  # print usage"
  , "       "++pn++" -v                  # print version"
  ]

