# Interpreter Foundational Design

**data** User

**data** Users: [ User ]

**data** System

**data** Systems: [ System ]

**data** DataType

**data** DataTypes: [ DataType ]

**data** Setting

**data** Scope

**data** Scenario

**data** Actor

**data** Event

**data** Action

**data** ScriptLine = Event | Action

**data** ActorScriptLine = Actor ScriptLine

**data** Script = [ ActorScriptLine ]

**data** EventPlayScript = EventPlayScript Users Systems DataTypes Setting Scope Script

# Interpreter Input Parsing Design

**data** InputFile

**data** InputText

**data** OutputText

**data** Filter

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
