{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Populate
    ( run
    ) where

import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.IO (FilePath)
import Text.Toml (parseTomlDoc)


-- | A URL (wrapper over Text)
newtype URL = URL T.Text deriving (IsString)

{- | Represents the information related to the source of a Song

Doesn't represent a song directly, but rather a source that may generate multiple
songs. One example usage would be to represent an album uploaded to a remote source,
and this would contain information about the author of the album, as well as timestamps
to split the album into multiple songs.
-}
data Source = Source
    { sourceName :: T.Text
    , sourceAuthor :: T.Text
    , sourceURL :: URL
    }
    
-- | Represents a complete configuration file of sources
newtype Sources = Sources [Source]


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
    let parseRes = parseTomlDoc file contents
    case parseRes of
        Left err -> print err
        Right _  -> putStrLn "This is a valid toml file"
