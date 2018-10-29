{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Populate.Sources
    ( URL(..)
    , Source(..)
    , Sources(..)
    , ConfigError(..)
    , ProgramError(..)
    , prettyProgramError
    , parseSources
    )
where

import Data.Foldable (foldl')
import qualified Data.HashMap.Lazy as HM
import Data.String (IsString)
import qualified Data.Text as T
import System.IO (FilePath)
import Text.Parsec.Error (ParseError)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types

import Util


-- | A URL (wrapper over Text)
newtype URL = URL T.Text deriving (Eq, IsString, Show)

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
    } deriving (Eq, Show)
    
-- | Represents a complete configuration file of sources
newtype Sources = Sources [Source] deriving (Eq, Show)

-- | Gets all the sources that compose this object
getSources :: Sources -> [Source]
getSources (Sources ss) = ss

-- | Prepend a source to the sources
addSource :: Source -> Sources -> Sources
addSource s (Sources ss) = Sources (s : ss)

-- | Represents the type of error for the program
data ProgramError 
    -- | An invalid toml file was provided
    = BadToml ParseError 
    -- | The toml file didn't match the expected config
    | BadConfig [ConfigError]
    deriving (Eq, Show)


-- | Represents errors generated while trying to match the config
data ConfigError
    -- | Invalid or missing name in nth entry
    = BadSourceName Int
    -- | Invalid or missing author in nth entry
    | BadSourceAuthor Int
    -- | Invalid or missing url in nth entry
    | BadSourceURL Int 
    -- | The configuration wasn't a top level list of tables
    | NotArrayOfTables
    deriving (Eq, Show)


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
        <> " key. It is either missing, or not text."
    prettyConfigError (BadSourceName i) = 
        missingInvalid i "name"
    prettyConfigError (BadSourceAuthor i) =
        missingInvalid i "author"
    prettyConfigError (BadSourceURL i) =
        missingInvalid i "url"
    prettyConfigError NotArrayOfTables =
        "The config file must be an array of [source] tables"


{- | Attempts to parse text into a valid source

The filepath is only needed to mention it in the ParseError
-}
parseSources :: T.Text -> FilePath -> Either ProgramError Sources
parseSources contents fileName = do
    toml <- mapErr BadToml (parseTomlDoc fileName contents)
    mapErr BadConfig (readToml toml)

-- | Attempts to read toml into Sources
readToml :: Table -> Either [ConfigError] Sources
readToml table = case HM.lookup "sources" table of
    Nothing               -> Left [NotArrayOfTables]
    Just (VTArray tables) -> snd $ foldl' validate (1, Right (Sources [])) tables
    Just _                -> Left [NotArrayOfTables]
  where
    checkNode :: ConfigError -> Maybe Node -> Either [ConfigError] T.Text
    checkNode err Nothing            = Left [err]
    checkNode _ (Just (VString txt)) = Right txt
    checkNode err (Just _)           = Left [err]
    tryLookup :: T.Text -> ConfigError -> Table -> Either [ConfigError] T.Text
    tryLookup key err = checkNode err . HM.lookup key
    bindErrs :: Either [e] (a -> b) -> Either [e] a -> Either [e] b
    bindErrs (Left errs1) (Left errs2) = 
        Left (errs1 ++ errs2)
    bindErrs l r = 
        l <*> r
    trySource :: Int -> Table -> Either [ConfigError] Source
    trySource i table = pure
        (\name author url -> Source name author (URL url))
        `bindErrs` tryLookup "name" (BadSourceName i) table
        `bindErrs` tryLookup "author" (BadSourceAuthor i) table
        `bindErrs` tryLookup "url" (BadSourceURL i) table
    validate (i, acc) node = (,) (i + 1) $
        pure addSource
        `bindErrs` trySource i node
        `bindErrs` acc
