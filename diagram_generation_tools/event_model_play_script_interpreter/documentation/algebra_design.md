# Interpreter Foundational Design

**data** User

**data** Users: [ User ]

**data** System

**data** Systems: [ System ]

**data** DataType

**data** DataTypes: [ DataType ]

**data** Setting

**data** Scope

**data** Script

**data** EventPlayScript = EventPlayScript Users Systems DataTypes Setting Scope Script

# Interpreter Input Parsing Design

**data** InputFile

**data** Filter

**data** UsersSection

**data** SystemsSection

**data** DataTypesSection

**data** SettingSection

**data** ScopeSection

**data** ScriptSection

**data** FileParseError

**data** PlayParseError

**data** FilterError

**data** ValidEventPlayText = ValidEventPlayText UsersSection SystemsSection DataTypesSection SettingSection ScopeSection ScriptSection

_parseInputFile :: InputFile -> Result(ValidEventPlayText, FileParseError)_

_parseValidEventPlayText :: ValidEventPlayText -> Result(EventPlayScript, PlayParseError)_

_applyFilter :: EventPlayScript -> Filter -> Result(EventPlayScript, FilterError)_

# Interpreter Output Displaying Design