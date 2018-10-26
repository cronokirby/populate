{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Populate
    ( run
    ) where

import Data.String (IsString)
import qualified Data.Text as T


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


run :: IO ()
run = putStrLn "someFunc"
