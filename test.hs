module Main where

import Paths_golfscript (getDataFileName)
import System.FilePath (takeDirectory)
import System.Directory (getDirectoryContents, doesFileExist, setCurrentDirectory)
import Data.Maybe (mapMaybe)
import Control.Monad (forM, (>=>))
import Data.List (stripPrefix)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

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
  case (rb, hs) of
    (Left _, Left _) -> return ()
    _                -> assertEqual prog rb hs

runRuby :: FilePath -> String -> IO (Either String String)
runRuby fprog input = do
  (c, o, e) <- readProcessWithExitCode "ruby" ["golfscript.rb", fprog] input
  return $ case c of
    ExitSuccess   -> Right o
    ExitFailure _ -> Left e

runHaskell :: FilePath -> String -> IO (Either String String)
runHaskell fprog input = do
  p <- fmap eval $ readFile fprog
  let golf = push (Str input) >> runs p >> fmap output stackToArr
  result <- runWrappedIO $ runGolf golf wrappedState
  return $ case result of
    (Left  err, _)   -> Left err
    (Right stk, out) -> Right $ out ++ stk ++ "\n"

wrappedState :: GolfState WrappedIO
wrappedState = emptyWith preludeWrappedIO
