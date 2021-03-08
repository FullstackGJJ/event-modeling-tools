module EventPlayScript.Script.Functions where

import Data.Text
import Data.Text.Conversions

import EventPlayScript.Script.Data
import qualified EventPlayScript.Script.InternalFunctions as I

-----------------Function Declarations-----------------
hideUnnecessaryLines :: Script -> FilterParameter -> Script
actorInScript :: Script -> String -> Bool
mentionedInScript :: Script -> String -> Bool

----------------Function Implementations----------------
hideUnnecessaryLines script filterParameter = Prelude.map (I.filterLine filterParameter) script 

actorInScript script actor = Prelude.any (I.actorInLine actor) script

mentionedInScript script word = Prelude.any (I.mentionedInLine word) script
