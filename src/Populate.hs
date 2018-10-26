module Populate
    ( run
    ) where

import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.IO (FilePath)

import Populate.Sources


-- | The entry point for the application.
run :: IO ()
run = do
    args <- getArgs
    case args of
        []    -> putStrLn "Please provide a toml file with a list of sources"
        (f:_) -> runOn f


-- | Runs the program with a given file
runOn :: FilePath -> IO ()
runOn file = do
    contents <- T.readFile file    
    let parseRes = parseSources contents file
    case parseRes of
        Left err -> T.putStrLn (prettyProgramError err)
        Right _  -> putStrLn "This is a valid config file"
