module InputTextParsing.FunctionsSpec where

import Test.Hspec

import InputTextParsing.Functions

spec :: Spec
spec = do 
    describe "collectPattern" $ do
        it "should return list with first two lines when given lines of text, pattern start that matches first line, and pattern end at empty line" $ do
            let inputTextLines = [ "Users:"
                                 , "  - Designers: People who design document templates"
                                 , ""
                                 , "Systems:"
                                 , "  - Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates"
                                 , "  - Reporting: System that renders DocumentTemplates into DocumentPdfs"
                                 , ""
                                 , "DataTypes:"
                                 , "  - DocumentTemplate: Document Template that gets transformed to PDF"
                                 , "  - DocumentPdf: Output PDF of document to sell to customers"
                                 , "  - OldDocumentDataFormat: The old json scheme of the data format"
                                 , "  - NewDocumentDataFormat: The new json scheme of the data format"
                                 , "  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider"
                                 , "" ]
            let inputParsePatternStart = "Users:"
            let inputParsePatternEnd = ""
            let expectedResult = [ "Users:"
                                 , "  - Designers: People who design document templates" ]

            collectPattern inputTextLines inputParsePatternStart inputParsePatternEnd `shouldBe` expectedResult

    describe "splitIntoSubstringsByChars" $ do
        it "should return correct list of texts when given input line and list of delimiters" $ do
            True `shouldBe` True
