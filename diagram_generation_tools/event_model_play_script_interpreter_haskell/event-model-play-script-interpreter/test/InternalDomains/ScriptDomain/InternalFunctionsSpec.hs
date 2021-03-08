module InternalDomains.ScriptDomain.InternalFunctionsSpec (spec) where

import Test.Hspec

import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.InternalFunctions

spec :: Spec
spec = do 
    describe "actorInLine" $ do
        it "correctly determines that actor is performing in the line" $ do
            let inputActor = "Designers"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "correctly determines that actor is mentioned in the line" $ do
            let inputActor = "Converter"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            actorInLine inputActor inputLine `shouldBe` expectedResult
        it "correctly determines that actor is not in the line" $ do
            let inputActor = "Reporting"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = False
            actorInLine inputActor inputLine `shouldBe` expectedResult

    describe "mentionedInLine" $ do
        it "correctly determines that word is mentioned in line" $ do
            let inputWord = "DocumentTemplate"
            let inputLine = Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
            let expectedResult = True
            mentionedInLine inputWord inputLine `shouldBe` expectedResult

    -- :: String -> ActorScriptLine -> Bool
    -- filterLine :: FilterParameter -> ActorScriptLine -> ActorScriptLine

    describe "filterLine" $ do
        it "correctly returns the original line that is not being filtered" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Converter"
            let expectedResult = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))

            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

        it "correctly returns blank line when given line that should be filtered" $ do
            let inputLine = Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate"))
            let inputFilterParameter = "Reporting"
            let expectedResult = HiddenLine "..."

            filterLine inputFilterParameter inputLine `shouldBe` expectedResult

