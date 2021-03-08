module EventPlayScript.Script.InternalFunctions where

import Data.Text
import Data.Text.Conversions

import EventPlayScript.Script.Data

-----------------Function Declarations-----------------
actorInLine :: String -> ActorScriptLine -> Bool
mentionedInLine :: String -> ActorScriptLine -> Bool
filterLine :: FilterParameter -> ActorScriptLine -> ActorScriptLine

----------------Function Implementations----------------
actorInLine actorName (HiddenLine _) = False
actorInLine actorName (Line actorInLine scriptLine) = 
    if toText actorName `isInfixOf` toText actorInLine
    then 
        True
    else
        case scriptLine of (Event (ReceivedBroadcast (REQUEST actorReference _ _ ))) -> toText actorName `isInfixOf` toText actorReference
                           (Event (ReceivedBroadcast (RESPONSE actorReference _ _))) -> toText actorName `isInfixOf` toText actorReference
                           (Broadcast (REQUEST actorReference _ _ )) -> toText actorName `isInfixOf` toText actorReference
                           (Broadcast (RESPONSE actorReference _ _)) -> toText actorName `isInfixOf` toText actorReference
                           (Event _) -> False

mentionedInLine word (HiddenLine _) = False
mentionedInLine word (Line _ scriptLine) =
    case scriptLine of (Event (ReceivedBroadcast (REQUEST _ _ parameters ))) -> toText word `isInfixOf` toText parameters
                       (Event (ReceivedBroadcast (RESPONSE _ _ response))) -> toText word `isInfixOf` toText response
                       (Event (Action actionContent)) -> toText word `isInfixOf` toText actionContent
                       (Broadcast (REQUEST _ _ parameters )) -> toText word `isInfixOf` toText parameters
                       (Broadcast (RESPONSE _ _ response)) -> toText word `isInfixOf` toText response

filterLine filterParameter (Line actor scriptLine) = 
    if toText actor `isInfixOf` toText filterParameter
    then Line actor scriptLine
    else HiddenLine "..."
filterLine filterParameter (HiddenLine hiddenSymbol) = HiddenLine hiddenSymbol
