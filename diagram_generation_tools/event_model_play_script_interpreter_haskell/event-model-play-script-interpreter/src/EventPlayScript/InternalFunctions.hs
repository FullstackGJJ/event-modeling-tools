module EventPlayScript.InternalFunctions where

import EventPlayScript.Data
import qualified EventPlayScript.Script.Data as SD
import qualified EventPlayScript.Script.Functions as SF

data SearchParameter = ActorName | LineContent

-----------------Function Declarations-----------------
filterDefinitionsByScript :: SD.Script -> [ Definition ] -> SearchParameter -> [ Definition ]

----------------Function Implementations----------------
filterDefinitionsByScript script definitions ActorName = filter (\x -> (SF.actorInScript script) (name x)) definitions
filterDefinitionsByScript script words LineContent = filter (\x -> (SF.mentionedInScript script) (name x)) words
