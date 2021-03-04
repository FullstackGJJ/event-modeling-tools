module Data where

-- Domain Types

type Name = String

type Description = String

data Definition = Definition { name::Name, description::Description }

type User = Definition

type Users = [ User ]

type System = Definition

type Systems = [ System ]

type DataType = Definition

type DataTypes = [ DataType ]

type Setting = String

type Scope = String

type Scenario = String

type Actor = String

data ScriptLineType = Event | Action

data ScriptLine = ScriptLine { lineType::ScriptLineType, content::String }

data ActorScriptLine = ActorScriptLine { actor::Actor, line::ScriptLine }

type Script = [ ActorScriptLine ]

data EventPlayScript = EventPlayScript { 
    users::Users,
    systems::Systems,
    dataTypes::DataTypes,
    setting::Setting,
    scope::Scope,
    script::Script
}

-- Text to Domain Transition types

type InputFile = String

type InputText = String

type OutputText = String

type Filter = String

type UsersSection = [ String ]

type SystemsSection = [ String ]

type DataTypesSection = [ String ]

type SettingSection = String

type ScopeSection = String

type ScenarioSection = String

type ScriptSection = [ String ]

type FileAccessError = String

type TextParsingError = String

type EventPlayValidationError = String

type PlayParseError = String

type FilterError = String

data RawEventPlayText = RawEventPlayText {
    usersSection::UsersSection,
    systemsSection::SystemsSection,
    dataTypesSection::DataTypesSection,
    settingSection::SettingSection,
    scopeSection::ScopeSection,
    scriptSection::ScriptSection
}

data ValidEventPlayText = ValidEventPlayText {
    validUsersSection::UsersSection,
    validSystemsSection::SystemsSection,
    validDataTypesSection::DataTypesSection,
    validSettingSection::SettingSection,
    validScopeSection::ScopeSection,
    validScriptSection::ScriptSection
}
