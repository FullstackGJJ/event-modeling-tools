{-# LANGUAGE MultiWayIf #-}
module InputTextParsing.InternalFunctions where

import InputTextParsing.Data

-----------------Function Declarations-----------------
addOrIgnoreLine :: PatternParseStart -> PatternParseEnd -> (Pattern, Bool) -> InputTextLine -> (Pattern, Bool)
addCharUnlessDelimiter :: ([String], [Char]) -> Char -> ([String], [Char])

----------------Function Implementations----------------
addOrIgnoreLine patternParseStart patternParseEnd acc inputTextLine = 
    if | not (snd acc) && inputTextLine == patternParseStart -> (fst acc, True)
       | (snd acc) && inputTextLine /= patternParseEnd -> ((fst acc) ++ [inputTextLine], (snd acc))
       | (snd acc) && inputTextLine == patternParseEnd -> (fst acc, False)
       | otherwise -> (fst acc, snd acc)

addCharUnlessDelimiter acc inputChar = 
    let (accStrings, accCharDelimiters) = acc
    in case accCharDelimiters of [] -> (((head accStrings) ++ [inputChar]) : (tail accStrings), accCharDelimiters)
                                 (x: xs) -> 
                                     if | x == inputChar -> ("" : accStrings, xs)
                                        | otherwise -> (((head accStrings) ++ [inputChar]) : (tail accStrings), accCharDelimiters)
