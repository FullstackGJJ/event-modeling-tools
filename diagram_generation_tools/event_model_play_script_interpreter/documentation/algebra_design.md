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

# Interpreter Input Parsing Design

**data** InputFile

**data** ValidTextFormat

**data** UsersSection

**data** SystemsSection

**data** DataTypesSection

**data** SettingSection

**data** ScopeSection

**data** ScriptSection

_validateAndRetrieveFileInput :: InputFile -> Option(ValidTextFormat)_

_parseValidTextFormat :: ValidTextFormat -> (UsersSection, SystemsSection, DataTypesSection, SettingSection, ScopeSection, ScriptSection)_
