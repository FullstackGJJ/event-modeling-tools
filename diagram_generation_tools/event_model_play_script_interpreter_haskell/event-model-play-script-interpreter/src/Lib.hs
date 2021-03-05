module Lib ( interpretAndFilterEventPlayScript
           , InputFile
           , Filter
           , OutputText ) where

import Data ( InputFile
            , Filter
            , OutputText )
import Functions ( 

interpretAndFilterEventPlayScript :: InputFile -> Filter -> OutputText

interpretAndFilterEventPlayScript inputFile filter = do
    
