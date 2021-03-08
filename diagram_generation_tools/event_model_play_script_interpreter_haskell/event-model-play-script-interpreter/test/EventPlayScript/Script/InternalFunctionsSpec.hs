module EventPlayScript.Script.InternalFunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Script.Data
import EventPlayScript.Script.InternalFunctions

spec :: Spec
spec = do 
    describe "actorInLine" $ do
        it "should return true when given actor name and line where actor is performing" $ do
            let inputActor = "Designers"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "should return true when given actor name and line where actor is mentioned by another actor" $ do
            let inputActor = "Converter"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "should return false when given actor name and line where actor is not mentioned in any way" $ do
            let inputActor = "Reporting"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = False
            actorInLine inputActor inputLine `shouldBe` expectedResult

    describe "mentionedInLine" $ do
        it "should return true when given a word and a line where word is mentioned" $ do
            let inputWord = "DocumentTemplate"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            mentionedInLine inputWord inputLine `shouldBe` expectedResult

        it "should return false when given a word and a line that doesn't have the word" $ do
            let inputWord = "SubstitutionRule"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = False
            mentionedInLine inputWord inputLine `shouldBe` expectedResult

    describe "filterLine" $ do
        it "should return the original line that is not being filtered when given actor name and a line where actor is performing" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Converter"
            let expectedResult = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

        it "should return blank line when given actor name and a line where actor is not performing" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Reporting"
            let expectedResult = HiddenLine "..."
            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

