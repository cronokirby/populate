{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Populate.Sources
    ( URL(..)
    , TimeStamp(..)
    , Source(..)
    , Sources(..)
    , ConfigError(..)
    , ProgramError(..)
    , prettyProgramError
    , parseSources
    , downloadSources
    )
where

import Control.Monad (forM_)
import Data.Foldable (foldl')
import qualified Data.HashMap.Lazy as HM
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.IO (FilePath)
import System.Process (readProcess)
import Text.Parsec.Error (ParseError)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types

import Util


-- | A URL (wrapper over Text)
newtype URL = URL T.Text deriving (Eq, IsString, Show)

-- | Convert a URL into a string
urlToString :: URL -> String
urlToString (URL txt) = T.unpack txt


{- | Represents a timestamp for a sub song

We use text for the time, because it exists as an argument
to ffmpeg. If the text is nonsense, things will simply blow up
when ffempg is called.
-}
data TimeStamp = TimeStamp T.Text T.Text deriving (Eq, Show)

{- | Represents the information related to the source of a Song

Doesn't represent a song directly, but rather a source that may generate multiple
songs. One example usage would be to represent an album uploaded to a remote source,
and this would contain information about the author of the album, as well as timestamps
to split the album into multiple songs.
-}
data Source = Source
    { sourceName :: T.Text
    , sourceArtist :: T.Text
    , sourcePath :: T.Text
    , sourceURL :: URL
    -- | We use an empty list to represent no timestamps
    , sourceStamps :: [TimeStamp]
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
    | BadSourceArtist Int
    -- | Invalid or missing path in nth entry
    | BadSourcePath Int
    -- | Invalid or missing url in nth entry
    | BadSourceURL Int 
    -- | The entry has a timestamps or namestamps entry, but not the other
    | MismatchedTimeStamps Int
    -- | The timestamp arrays have different lengths
    | BadTimeStampLengths Int
    -- | The timestamp arrays aren't text
    | BadTimeStampType Int
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
    prettyConfigError (BadSourceArtist i) =
        missingInvalid i "artist"
    prettyConfigError (BadSourcePath i) =
        missingInvalid i "path"
    prettyConfigError (BadSourceURL i) =
        missingInvalid i "url"
    prettyConfigError (MismatchedTimeStamps i) =
        "Entry #" <> textShow i <> " has a namestamps array or a timestamps array, but not both."
    prettyConfigError (BadTimeStampLengths i) =
        "Entry #" <> textShow i <> " has namestamps and timestamps of differing lengths."
    prettyConfigError (BadTimeStampType i) =
        "Entry #" <> textShow i <> " has timestamps that aren't text."
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
readToml table = case HM.lookup "source" table of
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
    tryTimeStamps :: Int -> Table -> Either [ConfigError] [TimeStamp]
    tryTimeStamps i table = 
        case (HM.lookup "timestamps" table, HM.lookup "namestamps" table) of
            (Nothing, Nothing)                         -> Right []
            (Just (VArray times), Just (VArray names)) ->
                if V.length times == V.length names
                    then validateNodes (V.zip times names)
                    else Left [BadTimeStampLengths i]
            _                                          -> Left [MismatchedTimeStamps i]
      where
        validateNodes nodes
            | V.null nodes = Right []
            | otherwise    = case V.head nodes of
                -- Arrays of nodes in TOML are homomorphic
                (VString _, VString _) -> 
                    Right . V.toList . fmap (\(VString t, VString n) -> TimeStamp t n) $ nodes
                _                      -> Left [BadTimeStampType i]
    trySource :: Int -> Table -> Either [ConfigError] Source
    trySource i table = pure
        (\name author path url timestamps -> 
            Source name author path (URL url) timestamps)
        `bindErrs` tryLookup "name" (BadSourceName i) table
        `bindErrs` tryLookup "artist" (BadSourceArtist i) table
        `bindErrs` tryLookup "path" (BadSourcePath i) table
        `bindErrs` tryLookup "url" (BadSourceURL i) table
        `bindErrs` tryTimeStamps i table
    validate (i, acc) node = (,) (i + 1) $
        pure addSource
        `bindErrs` trySource i node
        `bindErrs` acc


-- | Downloads each source one by one and puts them in a file
downloadSources :: Sources -> IO ()
downloadSources (Sources ss) =
    forM_ ss $ \source -> do
        let url = urlToString $ sourceURL source
            nameFormat = T.unpack $ 
                sourcePath source 
                <> sourceName source 
                <> ".m4a"
        T.putStrLn $
            "Downloading: " 
            <> sourceArtist source 
            <> " - " 
            <> sourceName source
        readProcess "youtube-dl" [url, "-x", "--audio-format", "m4a", "-o", nameFormat] ""
