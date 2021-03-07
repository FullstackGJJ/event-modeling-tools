module Internal.Script where

import Data.Text
import Data.Text.Conversions

type Actor = String

type ActionContent = String

type Method = String

type Parameters = String

type Response = String

type HiddenSymbol = String

data Broadcast = REQUEST Actor Method Parameters 
               | RESPONSE Actor Method Response deriving (Eq, Show)

data Event = ReceivedBroadcast Broadcast 
           | Action ActionContent deriving (Eq, Show)

data ScriptLine = Event Event
                | Broadcast Broadcast deriving (Eq, Show)

data ActorScriptLine = Line Actor ScriptLine
                     | HiddenLine HiddenSymbol deriving (Eq, Show)

type Script = [ ActorScriptLine ]

type FilterParameter = String

----------------------Public Functions------------------------------

hideUnnecessaryLines :: Script -> FilterParameter -> Script
actorInScript :: Script -> String -> Bool
mentionedInScript :: Script -> String -> Bool

hideUnnecessaryLines script filterParameter = 
    let lineFilter = filterLine filterParameter
    in Prelude.map lineFilter script 

actorInScript script actor = Prelude.any (actorInLine actor) script

mentionedInScript script word = Prelude.any (mentionedInLine word) script

---------------------Private Functions------------------------------

actorInLine :: String -> ActorScriptLine -> Bool
mentionedInLine :: String -> ActorScriptLine -> Bool
filterLine :: FilterParameter -> ActorScriptLine -> ActorScriptLine

actorInLine actorName (HiddenLine hiddenSymbol) = False
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

mentionedInLine word (HiddenLine hiddenSymbol) = False
mentionedInLine word (Line _ scriptLine) =
    case scriptLine of (Event (ReceivedBroadcast (REQUEST _ _ parameters ))) -> toText word `isInfixOf` toText parameters
                       (Event (ReceivedBroadcast (RESPONSE _ _ response))) -> toText word `isInfixOf` toText response
                       (Event (Action actionContent)) -> toText word `isInfixOf` toText actionContent
                       (Broadcast (REQUEST _ _ parameters )) -> toText word `isInfixOf` toText parameters
                       (Broadcast (RESPONSE _ _ response)) -> toText word `isInfixOf` toText response

filterLine filterParameter actorScriptLine = 
    case actorScriptLine of (Line actor _) -> if toText actor `isInfixOf` toText filterParameter then actorScriptLine else HiddenLine "..."
                            (HiddenLine hiddenSymbol) -> HiddenLine hiddenSymbol
