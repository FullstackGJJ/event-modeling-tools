module EventPlayScript.InternalFunctions where

import EventPlayScript.Data
import qualified EventPlayScript.Script.Data as SD
import qualified EventPlayScript.Script.Functions as SF

-----------------Function Declarations-----------------
filterActorProfilesByScript :: SD.Script -> [ ActorProfile ] -> [ ActorProfile ]
filterDataTypesByScript :: SD.Script -> [ DataType ] -> [ DataType ]

----------------Function Implementations----------------
filterActorProfilesByScript script definitions = filter (\x -> (SF.actorInScript script) (apName x)) definitions
filterDataTypesByScript script dataTypes = filter (\x -> (SF.mentionedInScript script) (dtName x)) dataTypes
