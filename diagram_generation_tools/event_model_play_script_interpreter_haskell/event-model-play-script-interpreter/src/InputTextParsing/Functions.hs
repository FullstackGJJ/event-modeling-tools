{-# LANGUAGE MultiWayIf #-}
module InputTextParsing.Functions where

import InputTextParsing.Data

-----------------Function Declarations-----------------
collectPattern :: InputTextLines -> PatternParseStart -> PatternParseEnd -> Pattern
splitIntoSubstringsByChars :: PatternLine -> [ Char ] -> [ String ]

----------------Function Implementations----------------
collectPattern inputTextLines patternParseStart patternParseEnd = 
   foldl (addOrIgnoreLine patternParseStart patternParseEnd) ([], False) inputTextLines

addOrIgnoreLine :: PatternParseStart -> PatternParseEnd -> (Pattern, Bool) -> InputTextLine -> (Pattern, Bool)
addOrIgnoreLine patternParseStart patternParseEnd acc inputTextLine = 
    if | not (snd acc) && inputTextLine == patternParseStart -> (fst acc, True)
       | (snd acc) && inputTextLine /= patternParseEnd -> ((fst acc) ++ inputTextLine, (snd acc))
       | (snd acc) && inputTextLine == patternParseEnd -> (fst acc, False)
       | otherwise -> (fst acc, snd acc)

splitIntoSubstringsByChars patternLine charsToSplitBy = [ patternLine ]
