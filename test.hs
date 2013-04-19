module Main where

import Paths_golfscript (getDataFileName)
import System.FilePath (takeDirectory, replaceDirectory)
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.List (stripPrefix)
import System.Process (readProcess)

main = do
  rb <- getDataFileName "test/golfscript.rb"
  let testDir = takeDirectory rb
  files <- getDirectoryContents testDir
  forM_ files $ \f -> fromMaybe (return ()) $ do
    g <- stripPrefix "test" f
    h <- stripPrefix "gorp." $ reverse g
    let j = "test" ++ reverse h
        fprog = replaceDirectory f testDir
        fin = replaceDirectory (j ++ ".in") testDir
        fout = replaceDirectory (j ++ ".out") testDir
    Just $ do
      b <- doesFileExist fin
      input <- if b then readFile fin else return ""
      rubyOut <- runRuby rb fprog input
      haskellOut <- runHaskell fprog input
      if rubyOut == haskellOut
        then putStrLn $ "Test " ++ f ++ " passed."
        else mapM_ putStrLn
          [ "FAILED: Test " ++ f
          , "<Ruby output>"
          , rubyOut
          , "<Haskell output>"
          , haskellOut
          ]

runRuby :: FilePath -> FilePath -> String -> IO String
runRuby rb fprog input = readProcess "ruby" [rb, fprog] input

runHaskell :: FilePath -> String -> IO String
runHaskell fprog input = undefined
