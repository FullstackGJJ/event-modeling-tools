module EventPlayScript.Script.FunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Script.Data
import EventPlayScript.Script.Functions

spec :: Spec
spec = do 
    describe "hideUnnecessaryLines" $ do
        it "should return expected script with correctly hidden lines when given script and actor name to filter on" $ do
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
        it "should return true when given an actor name and a script that the actor plays a part in" $ do
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

        it "should return true when given an actor name and a script where anyone mentions them" $ do
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

        it "should return false when given an actor name and a script where actor plays no part and is not mentioned" $ do
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
        it "should return true when given a datatype name and script that mentions it" $ do
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

        it "should return false when given datatype name and script that does not mention it" $ do
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
