module InternalDomains.ScriptDomain.Functions where

import Data.Text
import Data.Text.Conversions

import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.InternalFunctions

-----------------Function Declarations-----------------
hideUnnecessaryLines :: Script -> FilterParameter -> Script
actorInScript :: Script -> String -> Bool
mentionedInScript :: Script -> String -> Bool

----------------Function Implementations----------------
hideUnnecessaryLines script filterParameter = 
    Prelude.map (filterLine filterParameter) script 

actorInScript script actor = Prelude.any (actorInLine actor) script

mentionedInScript script word = Prelude.any (mentionedInLine word) script
