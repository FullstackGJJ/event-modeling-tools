module Internal.ScriptSpec (spec) where

import Test.Hspec

import Data
import Internal.Script

spec :: Spec
spec = do 
    describe "public functions" $ do
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

    describe "private functions" $ do
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

