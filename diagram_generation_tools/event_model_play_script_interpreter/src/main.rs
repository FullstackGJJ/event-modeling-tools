use quicli::prelude::*;
use structopt::StructOpt;

use event_model_play_script_interpreter::interpreter;


/// Load event model play script to generate specified output
#[derive(Debug, StructOpt)]
struct Cli {

    /// The file to read
    file: String,

    /// Which perspectives to filter
    #[structopt(long = "filters", short = "f", default_value = "")]
    filter: String,

    #[structopt(flatten)]
    verbosity: Verbosity,
}

fn main() -> CliResult {
    let args = Cli::from_args();
    let parse_input_file_result = interpreter::parse_input_file(args.file)
        .and_then(|input_text| parse_input_text(input_text))
        .and_then(|valid_play_text| ;
    match parse_input_file_result {
        Ok(valid_play_text) => Ok(valid_play_text),
        Err(err) => Err(err)
    };
    Ok(())
}
