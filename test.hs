module Main where

import Paths_golfscript (getDataFileName)
import System.FilePath (takeDirectory, replaceDirectory, takeFileName)
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_, when)
import Data.List (stripPrefix)
import System.Process (readProcess)
import Data.IORef

import Language.GolfScript.Base
import Language.GolfScript.Prelude hiding (rb)
import Language.GolfScript.Parse

main :: IO ()
main = do
  rb <- getDataFileName "test/golfscript.rb"
  let testDir = takeDirectory rb
  files <- getDirectoryContents testDir
  failure <- newIORef False
  forM_ files $ \f -> fromMaybe (return ()) $ do
    g <- stripPrefix "test" f
    h <- stripPrefix "gorp." $ reverse g
    let j = "test" ++ reverse h
        fprog = replaceDirectory f testDir
        fin = replaceDirectory (j ++ ".in") testDir
    Just $ do
      b <- doesFileExist fin
      input <- if b then readFile fin else return ""
      rubyOut <- runRuby rb fprog input
      haskellOut <- runHaskell fprog input
      if rubyOut == haskellOut
        then putStrLn $ "PASSED: Test " ++ f
        else mapM_ putStrLn
          [ "FAILED: Test " ++ f
          , "<Ruby output>"
          , rubyOut
          , "<Haskell output>"
          , haskellOut
          ] >> writeIORef failure True
  failed <- readIORef failure
  when failed $ error "At least one test failed."

runRuby :: FilePath -> FilePath -> String -> IO String
runRuby rb fprog input = readProcess "ruby" [rb, fprog] input

runHaskell :: FilePath -> String -> IO String
runHaskell fprog input = do
  p <- fmap eval $ readFile fprog
  let golf = push (Str input) >> runs p >> fmap output stackToArr
  result <- runWrappedIO $ runGolf golf wrappedState
  case result of
    (Left err, _) -> error $
      "ERROR in Haskell test " ++ takeFileName fprog ++ ": " ++ err
    (Right stk, out) -> return $ out ++ stk ++ "\n"

wrappedState :: GolfState WrappedIO
wrappedState = emptyWith preludeWrappedIO
