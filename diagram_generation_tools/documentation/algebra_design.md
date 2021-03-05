# Event Model Play Script Foundational Domain Design

**data** Name

**data** Description

**data** Definition = Definition Name Description

**data** User: Definition

**data** Users: [ User ]

**data** System: Definition

**data** Systems: [ System ]

**data** DataType: Definition

**data** DataTypes: [ DataType ]

**data** Setting

**data** Scope

**data** Scenario

**data** Actor

**data** ActionContent

**data** Broadcast = REQUEST Actor Method Parameters
                   | RESPOND Actor Method Response

**data** Event = ReceivedBroadcast Broadcast | Action ActionContent

**data** ScriptLine = Event | Broadcast

**data** ActorScriptLine = Actor ScriptLine

**data** Script = [ ActorScriptLine ]

**data** EventPlayScript = EventPlayScript Users Systems DataTypes Setting Scope Script

**data** Filter

_filter :: EventPlayScript -> Filter -> EventPlayScript _

# Interpreter Input Parsing Design

**data** InputFile

**data** InputText

**data** OutputText

**data** UsersSection

**data** SystemsSection

**data** DataTypesSection

**data** SettingSection

**data** ScopeSection

**data** ScenarioSection

**data** ScriptSection

**data** FileAccessError

**data** TextParsingError

**data** EventPlayValidationError

**data** PlayParseError

**data** FilterError

**data** RawEventPlayText = RawEventPlayText UsersSection SystemsSection DataTypesSection SettingSection ScopeSection ScriptSection

**data** ValidEventPlayText = ValidEventPlayText UsersSection SystemsSection DataTypesSection SettingSection ScopeSection ScriptSection

_parseInputFile :: InputFile -> Result(InputText, FileAccessError)_

_parseInputText :: InputText -> Result(RawEventPlayText, TextParsingError)_

_validateRawEventPlayText :: RawEventPlayText -> Result(ValidEventPlayText, EventPlayValidationError)_

_parseValidEventPlayText :: ValidEventPlayText -> Result(EventPlayScript, PlayParseError)_

_applyFilter :: EventPlayScript -> Filter -> Result(EventPlayScript, FilterError)_

_getEventPlayScriptText :: EventPlayScript -> ValidEventPlayText_

_getEventPlayOutputText :: ValidEventPlayText -> OutputText_
