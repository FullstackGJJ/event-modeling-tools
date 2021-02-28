use std::fs;

struct Definition {
    name: String,
    description: String
}

type User = Definition;

type System = Definition;

type DataType = Definition;

type Setting = String;

type Scope = String;

type Actor = String;

enum ScriptLine {
    Event,
    Action
}

struct ActorScriptLine {
    actor: Actor,
    script_line: ScriptLine
}

type Script = Vec<ActorScriptLine>;

pub struct EventPlayScript {
    users: Vec<User>,
    systems: Vec<System>,
    data_types: Vec<DataType>,
    setting: Setting,
    scope: Scope,
    script: Script
}

type InputFile = String;

type InputText = String;

type Filter = String;

type UsersSection = Vec<String>;

type SystemsSection = Vec<String>;

type DataTypesSection = Vec<String>;

type SettingSection = Vec<String>;

type ScopeSection = Vec<String>;

type ScriptSection = Vec<String>;

pub type FileParseError = String;

pub type TextParseError = String;

pub type PlayParseError = String;

pub type FilterError = String;

#[derive(Debug)]
pub struct ValidEventPlayText {
    users_section: UsersSection,
    systems_section: SystemsSection,
    data_types_section: DataTypesSection,
    setting_section: SettingSection,
    scope_section: ScopeSection,
    script_section: ScriptSection
}

pub fn parse_input_file(input_file: InputFile) -> Result<InputText, FileParseError> {
    match fs::read_to_string(input_file) {
        Ok(file_text) => Ok(file_text),
        Err(error) => Err("File parsing error".to_string())
    }
}

pub fn parse_input_text(input_text: InputText) -> Result<ValidEventPlayText, TextParseError> {
    Ok(ValidEventPlayText {
        users_section: vec!["".to_string()],
        systems_section: vec!["".to_string()],
        data_types_section: vec!["".to_string()],
        setting_section: vec!["".to_string()],
        scope_section: vec!["".to_string()],
        script_section: vec!["".to_string()]
    })
}

pub fn parse_valid_event_play_text(valid_event_play_text: ValidEventPlayText) -> Result<EventPlayScript, PlayParseError> {
    Err("Unimplemented".to_string())
}

pub fn apply_filter(event_play_script: EventPlayScript, filter: Filter) -> Result<EventPlayScript, FilterError> {
    Err("Unimplemented".to_string())
}

pub fn get_event_play_script_text(event_play_pcript: EventPlayScript) -> ValidEventPlayText {
    ValidEventPlayText {
        users_section: vec!["".to_string()],
        systems_section: vec!["".to_string()],
        data_types_section: vec!["".to_string()],
        setting_section: vec!["".to_string()],
        scope_section: vec!["".to_string()],
        script_section: vec!["".to_string()]
    }
}
