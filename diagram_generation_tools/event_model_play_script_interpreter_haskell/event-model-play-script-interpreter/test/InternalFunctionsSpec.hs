module InternalFunctionsSpec (spec) where

import Test.Hspec

import Data
import InternalFunctions
import InternalDomains.ScriptDomain.Data

spec :: Spec
spec = do 
    describe "filterDefinitionsByScript" $ do
        it "correctly filters actor names by actors involved in the script" $ do
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

        it "correctly filters data type names by what's mentioned in the script" $ do
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
