module FunctionsSpec (spec) where

import Test.Hspec

import Data
import Functions
import InternalFunctions
import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.Functions

spec :: Spec
spec = do 
    describe "filter" $ do
        it "should return properly filtered event play script when under normal circumstance" $ do
            let inputEventPlayScript = EventPlayScript {
                users = [ Definition { name = "Designers", description = "" } ],
                systems = [ Definition { name = "Converter", description = "" }
                          , Definition { name = "Reporting", description = "" } ],
                dataTypes = [ Definition { name = "DocumentTemplate", description = "" } 
                            , Definition { name = "DocumentPdf", description = "" } 
                            , Definition { name = "OldDocumentDataFormat", description = "" } 
                            , Definition { name = "NewDocumentDataFormat", description = "" } 
                            , Definition { name = "SubstitutionRule", description = "" } ],
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
                users = [ Definition { name = "Designers", description = "" } ],
                systems = [ Definition { name = "Converter", description = "" }
                          , Definition { name = "Reporting", description = "" } ],
                dataTypes = [ Definition { name = "DocumentTemplate", description = "" } 
                            , Definition { name = "DocumentPdf", description = "" } 
                            , Definition { name = "OldDocumentDataFormat", description = "" } 
                            , Definition { name = "NewDocumentDataFormat", description = "" } 
                            , Definition { name = "SubstitutionRule", description = "" } ],
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
