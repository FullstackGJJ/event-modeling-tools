module EventPlayScript.InternalFunctionsSpec (spec) where

import Test.Hspec

import EventPlayScript.Data
import EventPlayScript.InternalFunctions
import EventPlayScript.Script.Data

spec :: Spec
spec = do 
    describe "filterDefinitionsByScript" $ do
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

            let inputDefinitions = [ Definition { name = "Designers", description = "" } 
                                   , Definition { name = "Converter", description = "" }
                                   , Definition { name = "Reporting", description = "" }
                                   , Definition { name = "Repository", description = "" } ]

            let expectedResult = [ Definition { name = "Converter", description = "" }
                                 , Definition { name = "Reporting", description = "" } ]

            filterDefinitionsByScript inputScript inputDefinitions ActorName `shouldBe` expectedResult

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

            let inputDatatypes = [ Definition { name = "DocumentTemplate", description = "" } 
                                 , Definition { name = "DocumentPdf", description = "" }
                                 , Definition { name = "Success", description = "" }
                                 , Definition { name = "SubstitutionRule", description = "" } ]

            let expectedResult = [ Definition { name = "DocumentTemplate", description = "" }
                                 , Definition { name = "DocumentPdf", description = "" } ]

            filterDefinitionsByScript inputScript inputDatatypes LineContent `shouldBe` expectedResult
