module Internal.ScriptSpec (spec) where

import Test.Hspec

import Data
import Internal.Script

spec :: Spec
spec = do 
    describe "hideUnnecessaryLines" $ do
        it "hides the expected lines and gives back expected script" $do
            let inputScript = [ (Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat")))
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , (Line "Reporting" (Event (ReceivedBroadcast (REQUEST "Converter" "render" "awesomeJson:DocumentTemplate"))))
                              , (Line "Reporting" (Broadcast (RESPONSE "Converter" "render" "awesomePdf:DocumentPdf")))
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , (Line "Designers" (Event (ReceivedBroadcast (RESPONSE "Converter" "convert" "Success")))) ]

            let inputFilterParameter = "Converter"

            let expectedResult = [ HiddenLine "..."
                                 , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                                 , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                                 , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                                 , HiddenLine "..."
                                 , HiddenLine "..."
                                 , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                                 , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                                 , HiddenLine "..." ]

            hideUnnecessaryLines inputScript inputFilterParameter `shouldBe` expectedResult

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
