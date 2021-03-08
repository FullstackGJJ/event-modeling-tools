module Data where

import InternalDomains.ScriptDomain.Data

type Name = String

type Description = String

data Definition = Definition { name::Name, description::Description } deriving (Eq, Show)

type User = Definition

type Users = [ User ]

type System = Definition

type Systems = [ System ]

type DataType = Definition

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
    script::Script
} deriving (Eq, Show)
