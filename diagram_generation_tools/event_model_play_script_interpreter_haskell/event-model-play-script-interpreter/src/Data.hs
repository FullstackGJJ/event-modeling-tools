module Data where

import Types

data Definition = Definition { name::Name, description::Description }

data ScriptLineType = Event | Action

data ScriptLine = ScriptLine { lineType::ScriptLineType, content::String }

data ActorScriptLine = ActorScriptLine { actor::Actor, line::ScriptLine }

data EventPlayScript = EventPlayScript { 
    users::Users,
    systems::Systems,
    dataTypes::DataTypes,
    setting::Setting,
    scope::Scope,
    script::[ ActorScriptLine ]
}

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
