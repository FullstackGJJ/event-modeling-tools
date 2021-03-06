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


hideUnnecessaryLines :: Script -> FilterParameter -> Script
hideUnnecessaryLines script filterParameter = 
    let lineFilter = filterLine filterParameter
    in Prelude.map lineFilter script 

filterLine :: FilterParameter -> ActorScriptLine -> ActorScriptLine
filterLine filterParameter actorScriptLine = 
    case actorScriptLine of (Line actor _) -> if toText actor `isInfixOf` toText filterParameter then actorScriptLine else HiddenLine "..."
                            (HiddenLine hiddenSymbol) -> HiddenLine hiddenSymbol
