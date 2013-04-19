module Main where

import Paths_golfscript (getDataFileName)
import System.FilePath (takeDirectory)
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
        fin = j ++ ".in"
        fout = j ++ ".out"
    Just $ doesFileExist fin >>= \b -> if b
      then undefined -- TODO: test with input file
      else undefined -- TODO: test without input file

runRuby :: FilePath -> FilePath -> Maybe FilePath -> IO String
runRuby rb fprog fin = do
  input <- maybe (return "") readFile fin
  readProcess "ruby" [rb, fprog] input
