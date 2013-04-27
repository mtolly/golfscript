module Main where

import Paths_golfscript (getDataFileName)
import System.FilePath (takeDirectory)
import System.Directory (getDirectoryContents, doesFileExist, setCurrentDirectory)
import Data.Maybe (mapMaybe)
import Control.Monad (forM, (>=>))
import Data.List (stripPrefix)
import System.Process (readProcess)

import Test.HUnit

import Language.GolfScript.Base
import Language.GolfScript.Prelude
import Language.GolfScript.Parse

main :: IO ()
main = do
  rb <- getDataFileName "test/golfscript.rb"
  setCurrentDirectory $ takeDirectory rb
  tests <- getTests
  result <- runTestTT $ TestList $ map (uncurry makeTest) tests
  if errors result + failures result /= 0
    then error "At least 1 test failed."
    else return ()

isTest :: FilePath -> Maybe FilePath
isTest = stripPrefix "test" >=> stripSuffix ".gs" where
  stripSuffix sfx str = fmap reverse $ stripPrefix (reverse sfx) (reverse str)

getTests :: IO [(FilePath, String)]
getTests = do
  tests <- fmap (mapMaybe isTest) $ getDirectoryContents "."
  forM tests $ \tst -> do
    let program = "test" ++ tst ++ ".gs"
        input   = "test" ++ tst ++ ".in"
    b <- doesFileExist input
    if b then readFile input >>= \i -> return (program, i)
      else return (program, "")

makeTest :: FilePath -> String -> Test
makeTest prog input = TestCase $ do
  rb <- runRuby prog input
  hs <- runHaskell prog input
  assertEqual prog rb hs

runRuby :: FilePath -> String -> IO String
runRuby fprog input = readProcess "ruby" ["golfscript.rb", fprog] input

runHaskell :: FilePath -> String -> IO String
runHaskell fprog input = do
  p <- fmap eval $ readFile fprog
  let golf = push (Str input) >> runs p >> fmap output stackToArr
  result <- runWrappedIO $ runGolf golf wrappedState
  case result of
    (Left err,  _)   -> error $ fprog ++ "\nHaskell error: " ++ err
    (Right stk, out) -> return $ out ++ stk ++ "\n"

wrappedState :: GolfState WrappedIO
wrappedState = emptyWith preludeWrappedIO
