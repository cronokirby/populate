{- |
Description: Exports ubiquitously used helper functions.

This is necessary to patch some holes in the interfaces
of other libraries.
-}
module Util
    ( mapErr
    , textShow
    )
where

import qualified Data.Text as T


-- | Maps over the left side of an Either
mapErr :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapErr f = either (Left . f) Right


-- | Show something as Text
textShow :: Show s => s -> T.Text
textShow = T.pack . show
