{-# LANGUAGE OverloadedStrings #-}
import Data.Either (isLeft)
import Test.Hspec

import Populate.Sources


main :: IO ()
main = hspec sourcesSpec


sourcesSpec = do
    describe "parseSources" $ do
        it "returns an error for invalid toml files" $ do
            isLeft (parseSources "[sources]]" "") 
                `shouldBe` True
        it "returns an error for no sources" $ do
            parseShouldBe "" $
                Left (BadConfig [NotArrayOfTables])
        it "returns an error for wrong initial key" $ do
            parseShouldBe "[[sourzes]]" $
                Left (BadConfig [NotArrayOfTables])
        it "returns an error if the file doesn't start with an array" $ do
            parseShouldBe "[sources]" $
                Left (BadConfig [NotArrayOfTables])
        it "returns correctly indexed errors for missing fields" $ do
            parseShouldBe "[[sources]]\nname=\"a\"\nauthor=\"b\"" $
                Left (BadConfig [BadSourceURL 1])
            parseShouldBe "[[sources]]" . Left . BadConfig $
                [ BadSourceName 1
                , BadSourceAuthor 1
                , BadSourceURL 1
                ]
        it "parses valid source files" $ do
            parseShouldBe "[[sources]]\nname=\"a\"\nauthor=\"b\"\nurl=\"c\"" $ do
                Right (Sources [Source "a" "b" "c"])
  where
    parseShouldBe txt = shouldBe (parseSources txt "")
