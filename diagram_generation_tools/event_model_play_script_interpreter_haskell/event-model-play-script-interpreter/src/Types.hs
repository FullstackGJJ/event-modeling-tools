module Types where

-- Domain Types

type Name = String

type Description = String

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
