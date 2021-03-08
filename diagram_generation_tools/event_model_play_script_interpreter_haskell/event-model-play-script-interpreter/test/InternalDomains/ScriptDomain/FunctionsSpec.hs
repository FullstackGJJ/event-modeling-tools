module InternalDomains.ScriptDomain.FunctionsSpec (spec) where

import Test.Hspec

import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.Functions

spec :: Spec
spec = do 
    describe "hideUnnecessaryLines" $ do
        it "hides the expected lines and gives back expected script" $ do
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
        it "correctly determines that the actor has a line in the script" $ do
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

        it "correctly determines that the actor is mentioned in the script" $ do
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

        it "correctly determines that the actor is not in the script" $ do
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
        it "correctly determines that the datatype is in the script" $ do
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

        it "correctly determines that the datatype is not in the script" $ do
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
