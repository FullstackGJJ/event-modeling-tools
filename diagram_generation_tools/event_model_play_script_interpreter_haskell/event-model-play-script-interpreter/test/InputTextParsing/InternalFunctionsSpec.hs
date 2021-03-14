module InputTextParsing.InternalFunctionsSpec where

import Test.Hspec

import InputTextParsing.Data
import InputTextParsing.InternalFunctions as I

spec :: Spec
spec = do 
    describe "addOrIgnoreLine" $ do
        it "should return accumulator with appended line and true when given input accumulator that is set to accumulate to true" $ do
            let inputPatternParseStart = "Users:"
            let inputPatternParseEnd = ""
            let inputAcc = ([], True)
            let inputTextLine =  "  - Designers: People who design document templates"
            let expectedResult = (["  - Designers: People who design document templates"], True)

            I.addOrIgnoreLine inputPatternParseStart inputPatternParseEnd inputAcc inputTextLine `shouldBe` expectedResult

        it "should return accumulator with nothing and false when given input accumulator and input line that matches pattern end" $ do
            let inputPatternParseStart = "Users:"
            let inputPatternParseEnd = ""
            let inputAcc = ([], True)
            let inputTextLine =  ""
            let expectedResult = ([], False)

            I.addOrIgnoreLine inputPatternParseStart inputPatternParseEnd inputAcc inputTextLine `shouldBe` expectedResult

        it "should return accumulator with nothing and true when given input accumulator and input line that matches pattern start" $ do
            let inputPatternParseStart = "Users:"
            let inputPatternParseEnd = ""
            let inputAcc = ([], False)
            let inputTextLine =  "Users:"
            let expectedResult = ([], True)

            I.addOrIgnoreLine inputPatternParseStart inputPatternParseEnd inputAcc inputTextLine `shouldBe` expectedResult

    describe "addCharUnlessDelimiter" $ do
        it "should return acc with appended char on head of list and unchanged char list when given char that's not equal to delimiter" $ do
            let inputAcc = ([""], [':'])
            let inputChar = 'b'
            let expectedResult = (["b"], [':'])

            I.addCharUnlessDelimiter inputAcc inputChar `shouldBe` expectedResult

        it "should return acc with appended new head of list and popped char delimiter list when given char that is equal to delimiter" $ do
            let inputAcc = (["bob"], [':'])
            let inputChar = ':'
            let expectedResult = (["", "bob"], [])

            I.addCharUnlessDelimiter inputAcc inputChar `shouldBe` expectedResult

        it "should return acc with appended char on head of list and empty delimiter list when given char delimiter list is empty" $ do
            let inputAcc = (["bob"], [])
            let inputChar = 'b'
            let expectedResult = (["bobb"], [])

            I.addCharUnlessDelimiter inputAcc inputChar `shouldBe` expectedResult
