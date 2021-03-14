module EventPlayScript.Data where

import qualified EventPlayScript.Script.Data as SD

type Name = String

type Description = String

data ActorProfile = ActorProfile { apName::Name, apDescription::Description } deriving (Eq, Show)

type User = ActorProfile

type Users = [ User ]

type System = ActorProfile

type Systems = [ System ]

data DataType = DataType { dtName::Name, dtDescription::Description } deriving (Eq, Show)

type DataTypes = [ DataType ]

type Setting = String

type Scope = String

type Scenario = String

data EventPlayScript = EventPlayScript { 
    users::Users,
    systems::Systems,
    dataTypes::DataTypes,
    setting::Setting,
    scope::Scope,
    scenario::Scenario,
    script::SD.Script
} deriving (Eq, Show)
