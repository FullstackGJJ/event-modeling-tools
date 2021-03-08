module EventPlayScript.Script.InternalFunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Script.Data
import EventPlayScript.Script.InternalFunctions

spec :: Spec
spec = do 
    describe "actorInLine" $ do
        it "should determine that actor is performing in the line when actor is performing the line" $ do
            let inputActor = "Designers"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "should determine that actor is mentioned in the line when is mentioned by another actor" $ do
            let inputActor = "Converter"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "should determine that actor is not in the line when is not in there in any way" $ do
            let inputActor = "Reporting"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = False
            actorInLine inputActor inputLine `shouldBe` expectedResult

    describe "mentionedInLine" $ do
        it "should determine that the word is mentioned in line when the word is mentioned" $ do
            let inputWord = "DocumentTemplate"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            mentionedInLine inputWord inputLine `shouldBe` expectedResult

        it "should determine that the word is not mentioned in line when the line doesn't have the line" $ do
            let inputWord = "SubstitutionRule"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = False
            mentionedInLine inputWord inputLine `shouldBe` expectedResult

    describe "filterLine" $ do
        it "should return the original line that is not being filtered when parameter dictates to not filter" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Converter"
            let expectedResult = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

        it "should return blank line when paramter determines line should be filtered" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Reporting"
            let expectedResult = HiddenLine "..."
            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

