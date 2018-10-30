{-# LANGUAGE OverloadedStrings #-}
import Data.Either (isLeft)
import qualified Data.Text.IO as T
import Test.Hspec

import Populate.Sources


main :: IO ()
main = hspec sourcesSpec


sourcesSpec =
    describe "parseSources" $ do
        it "returns an error for invalid toml files" $
            isLeft (parseSources "[source]]" "") 
                `shouldBe` True
        it "returns an error for no sources" $
            parseShouldBe "" $
                Left (BadConfig [NotArrayOfTables])
        it "returns an error for wrong initial key" $
            parseShouldBe "[[sourze]]" $
                Left (BadConfig [NotArrayOfTables])
        it "returns an error if the file doesn't start with an array" $
            parseShouldBe "[source]" $
                Left (BadConfig [NotArrayOfTables])
        it "returns correctly indexed errors for missing fields" $ do
            parseShouldBe "[[source]]\nname=\"a\"\nartist=\"b\"" $
                Left (BadConfig [BadSourcePath 1, BadSourceURL 1])
            parseShouldBe "[[source]]" . Left . BadConfig $
                [ BadSourceName 1
                , BadSourceArtist 1
                , BadSourcePath 1
                , BadSourceURL 1
                ]
        it "parses valid source files" $ do
            content <- T.readFile "examples/example.toml"
            parseShouldBe content $
                Right . Sources $ 
                    [ Source 
                      "Gattsu"
                      "Susume Hirasawa"
                      "Susume Hirasawa/"
                      "https://www.youtube.com/watch?v=_isSnrC2__A"
                      [TimeStamp "0:00" "first", TimeStamp "1:00" "second"]
                    ]
  where
    parseShouldBe txt = shouldBe (parseSources txt "")
