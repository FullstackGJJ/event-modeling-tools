module InputTextParsing.Functions where

import Data.Char
import Data.String.Utils

import InputTextParsing.Data
import InputTextParsing.InternalFunctions as I

-----------------Function Declarations-----------------
collectPattern :: InputTextLines -> PatternParseStart -> PatternParseEnd -> Pattern
splitIntoSubstringsByChars :: PatternLine -> [ Char ] -> [ String ]

----------------Function Implementations----------------
collectPattern inputTextLines patternParseStart patternParseEnd = 
   let (result, _) = foldl (I.addOrIgnoreLine patternParseStart patternParseEnd) ([], False) inputTextLines
   in result

splitIntoSubstringsByChars patternLine charsToSplitBy = 
    let (result, _) = foldl (I.addCharUnlessDelimiter) ([""], charsToSplitBy) patternLine
        reversedResult = reverse result
        prunedResult = map (\x -> strip x) reversedResult
        filteredResult = filter (\x -> not (all isSpace x)) prunedResult
    in filteredResult
