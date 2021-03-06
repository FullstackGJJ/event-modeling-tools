module EventPlayScript.FunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Data
import EventPlayScript.Functions as Functions
import EventPlayScript.Script.Data
import EventPlayScript.Script.Functions

spec :: Spec
spec = do 
    describe "filter" $ do
        it "should return expected filtered event play script when given event play script and actor name to filter on" $ do
            let inputEventPlayScript = EventPlayScript {
                users = [ ActorProfile { apName = "Designers", apDescription = "" } ],
                systems = [ ActorProfile { apName = "Converter", apDescription = "" }
                          , ActorProfile { apName = "Reporting", apDescription = "" } ],
                dataTypes = [ DataType { dtName = "DocumentTemplate", dtDescription = "" } 
                            , DataType { dtName = "DocumentPdf", dtDescription = "" } 
                            , DataType { dtName = "OldDocumentDataFormat", dtDescription = "" } 
                            , DataType { dtName = "NewDocumentDataFormat", dtDescription = "" } 
                            , DataType { dtName = "SubstitutionRule", dtDescription = "" } ],
                setting = "Test setting",
                scope = "Test scope",
                scenario = "Test scenario",
                script = [ (Line "Designers" (Broadcast (REQUEST "Converter" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat")))
                         , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                         , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                         , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                         , (Line "Reporting" (Event (ReceivedBroadcast (REQUEST "Converter" "render" "awesomeJson:DocumentTemplate"))))
                         , (Line "Reporting" (Broadcast (RESPONSE "Converter" "render" "awesomePdf:DocumentPdf")))
                         , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                         , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                         , (Line "Designers" (Event (ReceivedBroadcast (RESPONSE "Converter" "convert" "Success")))) ]
            }
            let inputFilterParameter = "Converter"
            let expectedResult = EventPlayScript {
                users = [ ActorProfile { apName = "Designers", apDescription = "" } ],
                systems = [ ActorProfile { apName = "Converter", apDescription = "" }
                          , ActorProfile { apName = "Reporting", apDescription = "" } ],
                dataTypes = [ DataType { dtName = "DocumentTemplate", dtDescription = "" } 
                            , DataType { dtName = "DocumentPdf", dtDescription = "" } 
                            , DataType { dtName = "OldDocumentDataFormat", dtDescription = "" } 
                            , DataType { dtName = "NewDocumentDataFormat", dtDescription = "" } 
                            , DataType { dtName = "SubstitutionRule", dtDescription = "" } ],
                setting = "Test setting",
                scope = "Test scope",
                scenario = "Test scenario",
                script = [ HiddenLine "..."
                         , (Line "Converter" (Event (ReceivedBroadcast (REQUEST "Designers" "convert" "awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))))
                         , (Line "Converter" (Event (Action "apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")))
                         , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                         , HiddenLine "..."
                         , HiddenLine "..."
                         , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                         , (Line "Converter" (Broadcast (RESPONSE "Designers" "convert" "SUCCESS")))
                         , HiddenLine "..." ]
            }
            Functions.filter inputEventPlayScript inputFilterParameter `shouldBe` expectedResult
