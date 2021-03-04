module FunctionsSpec (spec) where

import Test.Hspec

import Functions

spec :: Spec
spec = do 
    describe "parseInputFile" $ do
        it "returns back file contents" $ do
            parseInputFile "fake.txt" `shouldBe` Right "defintely not this"
