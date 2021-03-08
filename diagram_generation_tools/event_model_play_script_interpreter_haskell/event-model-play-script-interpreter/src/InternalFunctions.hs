module InternalFunctions where

import Data
import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.Functions

data SearchParameter = ActorName | LineContent

-----------------Function Declarations-----------------
filterDefinitionsByScript :: Script -> [ Definition ] -> SearchParameter -> [ Definition ]

----------------Function Implementations----------------
filterDefinitionsByScript script definitions ActorName = filter (\x -> (actorInScript script) (name x)) definitions
filterDefinitionsByScript script words LineContent = filter (\x -> (mentionedInScript script) (name x)) words
