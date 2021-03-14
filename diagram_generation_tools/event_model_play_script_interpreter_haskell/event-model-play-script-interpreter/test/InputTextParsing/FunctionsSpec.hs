module InputTextParsing.FunctionsSpec where

import Test.Hspec

import InputTextParsing.Data
import InputTextParsing.Functions

spec :: Spec
spec = do 
    describe "collectPattern" $ do
        it "should return only systems lines when given lines of text, pattern start at systems, and pattern end at empty line" $ do
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
                                 , "  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider" ]
            let inputParsePatternStart = "Systems:"
            let inputParsePatternEnd = ""
            let expectedResult = [ "  - Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates"
                                 , "  - Reporting: System that renders DocumentTemplates into DocumentPdfs" ]

            collectPattern inputTextLines inputParsePatternStart inputParsePatternEnd `shouldBe` expectedResult

        it "should return only data types lines when given lines of text, pattern start at data types, and pattern end at empty line" $ do
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
                                 , "  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider" ]
            let inputParsePatternStart = "DataTypes:"
            let inputParsePatternEnd = ""
            let expectedResult = [ "  - DocumentTemplate: Document Template that gets transformed to PDF"
                                 , "  - DocumentPdf: Output PDF of document to sell to customers"
                                 , "  - OldDocumentDataFormat: The old json scheme of the data format"
                                 , "  - NewDocumentDataFormat: The new json scheme of the data format"
                                 , "  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider" ]

            collectPattern inputTextLines inputParsePatternStart inputParsePatternEnd `shouldBe` expectedResult

    describe "splitIntoSubstringsByChars" $ do
        it "should return correct list of texts when given input line and list of delimiters" $ do
            let inputPatternLine = "Designers: People who design document templates"
            let inputCharList = [ ':' ]
            let expectedResult = [ "Designers"
                                 , "People who design document templates" ]
            
            splitIntoSubstringsByChars inputPatternLine inputCharList `shouldBe` expectedResult

        it "should return correct list of texts when given input line with delimiters of dashes and colons" $ do
            let inputPatternLine = " - Designers: People who design document templates"
            let inputCharList = [ '-', ':' ]
            let expectedResult = [ "Designers"
                                 , "People who design document templates" ]
            
            splitIntoSubstringsByChars inputPatternLine inputCharList `shouldBe` expectedResult
