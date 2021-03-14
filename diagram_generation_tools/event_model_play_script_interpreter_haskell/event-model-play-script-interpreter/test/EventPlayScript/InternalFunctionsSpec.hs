module EventPlayScript.InternalFunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Data
import EventPlayScript.InternalFunctions
import EventPlayScript.Script.Data

spec :: Spec
spec = do 
    describe "filterActorProfilesByScript" $ do
        it "should only return actor names involved in the script when given script and actor names" $ do
            let inputScript = [ HiddenLine "..."
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , (Line "Reporting" (Event (ReceivedBroadcast (REQUEST "Converter" "render" "awesomeJson:DocumentTemplate"))))
                              , (Line "Reporting" (Broadcast (RESPONSE "Converter" "render" "awesomePdf:DocumentPdf")))
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , HiddenLine "..."
                              , HiddenLine "..." ]

            let inputActorProfiles = [ ActorProfile { apName = "Designers", apDescription = "" } 
                                     , ActorProfile { apName = "Converter", apDescription = "" }
                                     , ActorProfile { apName = "Reporting", apDescription = "" }
                                     , ActorProfile { apName = "Repository", apDescription = "" } ]

            let expectedResult = [ ActorProfile { apName = "Converter", apDescription = "" }
                                 , ActorProfile { apName = "Reporting", apDescription = "" } ]

            filterActorProfilesByScript inputScript inputActorProfiles `shouldBe` expectedResult

        it "should only return data type names that are mentioned in the script when given script and data type names" $ do
            let inputScript = [ HiddenLine "..."
                              , HiddenLine "..."
                              , HiddenLine "..."
                              , (Line "Converter" (Broadcast (REQUEST "Reporting" "render" "awesomeJson:DocumentTemplate")))
                              , (Line "Reporting" (Event (ReceivedBroadcast (REQUEST "Converter" "render" "awesomeJson:DocumentTemplate"))))
                              , (Line "Reporting" (Broadcast (RESPONSE "Converter" "render" "awesomePdf:DocumentPdf")))
                              , (Line "Converter" (Event (ReceivedBroadcast (RESPONSE "Reporting" "render" "awesomePdf:DocumentPdf"))))
                              , HiddenLine "..."
                              , HiddenLine "..." ]

            let inputDatatypes = [ DataType { dtName = "DocumentTemplate", dtDescription = "" } 
                                 , DataType { dtName = "DocumentPdf", dtDescription = "" }
                                 , DataType { dtName = "Success", dtDescription = "" }
                                 , DataType { dtName = "SubstitutionRule", dtDescription = "" } ]

            let expectedResult = [ DataType { dtName = "DocumentTemplate", dtDescription = "" }
                                 , DataType { dtName = "DocumentPdf", dtDescription = "" } ]

            filterDataTypesByScript inputScript inputDatatypes `shouldBe` expectedResult
