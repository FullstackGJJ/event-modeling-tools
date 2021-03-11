# Script Domain

**data** Actor

**data** ActionContent

**data** Method

**data** Parameters

**data** Response

**data** HiddenSymbol

**data** Broadcast = REQUEST Actor Method Parameters | RESPONSE Actor Method Response

**data** Event = ReceivedBroadcast Broadcast | Action ActionContent

**data** ScriptLine = Event Event | Broadcast Broadcast

**data** ActorScriptLine = Line Actor ScriptLine | HiddenLine HiddenSymbol

**data** Script = [ ActorScriptLine ]

**data** FilterParameter

_hideUnnecessaryLines :: Script -> FilterParameter -> Script_

_actorInScript :: Script -> String -> Bool_

_mentionedInScript :: Script -> String -> Bool_
