module EventPlayScript.Script.FunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Script.Data
import EventPlayScript.Script.Functions

spec :: Spec
spec = do 
    describe "hideUnnecessaryLines" $ do
        it "should hide the expected lines and gives back expected script when filtering on input actor name" $ do
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
    describe "actorInScript" $ do
        it "should determine that the actor has a line in the script when given a line that the actor is assigned to" $ do
            let inputScript = [ HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , HiddenLine "..." ]
            let inputActor = "Converter"
            let expectedResult = True
            actorInScript inputScript inputActor `shouldBe` expectedResult

        it "should determine that the actor is mentioned in the script when anyone mentions them" $ do
            let inputScript = [ HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , HiddenLine "..." ]
            let inputActor = "Designers"
            let expectedResult = True
            actorInScript inputScript inputActor `shouldBe` expectedResult

        it "determine that the actor is not in the script when actor plays no part" $ do
            let inputScript = [ HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , HiddenLine "..." ]
            let inputActor = "Repository"
            let expectedResult = False
            actorInScript inputScript inputActor `shouldBe` expectedResult

    describe "mentionedInScript" $ do
        it "should determine that the datatype is in the script when datatype in mentioned" $ do
            let inputScript = [ HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , HiddenLine "..." ]
            let inputDataType = "DocumentPdf"
            let expectedResult = True
            mentionedInScript inputScript inputDataType `shouldBe` expectedResult

        it "should determines that the datatype is not in the script when datatype is not mentioned anywhere" $ do
            let inputScript = [ HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                              , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                              , HiddenLine "..." ]
            let inputDataType = "SuperDocument"
            let expectedResult = False
            mentionedInScript inputScript inputDataType `shouldBe` expectedResult
