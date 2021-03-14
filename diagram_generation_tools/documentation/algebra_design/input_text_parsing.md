# Input Text Parsing Domain

**data** InputTextLine

**data** InputTextLines = [ InputTextLine ]

**data** = PatternLine

**data** Pattern = [ PatternLine ]

**data** PatternParseStart

**data** PatternParseEnd

_collectPattern :: InputTextLines -> PatternParseStart -> PatternParseEnd -> Pattern_

_splitIntoSubstringsByChars :: PatternLine -> [ Char ] -> [ String ]_
