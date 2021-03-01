pub struct Definition {
    name: String,
    description: String
}

pub type User = Definition;

pub type System = Definition;

pub type DataType = Definition;

pub type Setting = String;

pub type Scope = String;

pub type Scenario = String;

pub type Actor = String;

pub enum ScriptLine {
    Event,
    Action
}

pub struct ActorScriptLine {
    pub actor: Actor,
    pub script_line: ScriptLine
}

pub type Script = Vec<ActorScriptLine>;

pub struct EventPlayScript {
    pub users: Vec<User>,
    pub systems: Vec<System>,
    pub data_types: Vec<DataType>,
    pub setting: Setting,
    pub scope: Scope,
    pub scenario: Scenario,
    pub script: Script
}

pub type InputFile = String;

pub type InputText = String;

pub type OutputText = String;

pub type Filter = String;

pub type UsersSection = Vec<String>;

pub type SystemsSection = Vec<String>;

pub type DataTypesSection = Vec<String>;

pub type SettingSection = String;

pub type ScopeSection = String;

pub type ScenarioSection = String;

pub type ScriptSection = Vec<String>;

pub type FileParseError = String;

pub type TextParseError = String;

pub type PlayParseError = String;

pub type FilterError = String;

#[derive(Debug, PartialEq)]
pub struct ValidEventPlayText {
    pub users_section: UsersSection,
    pub systems_section: SystemsSection,
    pub data_types_section: DataTypesSection,
    pub setting_section: SettingSection,
    pub scope_section: ScopeSection,
    pub scenario_section: ScenarioSection,
    pub script_section: ScriptSection
}

