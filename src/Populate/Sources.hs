{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Populate.Sources
    ( URL(..)
    , Source(..)
    , Sources(..)
    , ProgramError
    , prettyProgramError
    , parseSources
    )
where

import Data.String (IsString)
import qualified Data.Text as T
import System.IO (FilePath)
import Text.Parsec.Error (ParseError)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types

import Util


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


-- | Represents the type of error for the program
data ProgramError 
    -- | An invalid toml file was provided
    = BadToml ParseError 
    -- | The toml file didn't match the expected config
    | BadConfig [ConfigError]


-- | Represents errors generated while trying to match the config
data ConfigError
    -- | Invalid or missing name in nth entry
    = BadSourceName Int
    -- | Invalid or missing author in nth entry
    | BadSourceAuthor Int
    -- | Invalid or missing url in nth entry
    | BadSourceURL Int 


-- | Provides a textual error for the CLI user
prettyProgramError :: ProgramError -> T.Text
prettyProgramError (BadToml parseError) =
    "Invalid toml file:\n" <> T.pack (show parseError)
prettyProgramError (BadConfig configErrors) = 
    "Invalid configuration:\n"
    <> T.intercalate "\n" (map prettyConfigError configErrors)
  where
    missingInvalid i t = 
        "Entry #"
        <> textShow i
        <> " must have a " 
        <> t 
        <> "key. It is either missing, or not text."
    prettyConfigError (BadSourceName i) = 
        missingInvalid i "name"
    prettyConfigError (BadSourceAuthor i) =
        missingInvalid i "author"
    prettyConfigError (BadSourceURL i) =
        missingInvalid i "url"


{- | Attempts to parse text into a valid source

The filepath is only needed to mention it in the ParseError
-}
parseSources :: T.Text -> FilePath -> Either ProgramError Sources
parseSources contents fileName = do
    toml <- mapErr BadToml (parseTomlDoc fileName contents)
    mapErr BadConfig (readToml toml)

-- TODO: implement this
-- | Attempts to read toml into Sources
readToml :: Table -> Either [ConfigError] Sources
readToml _ = Left [] 
