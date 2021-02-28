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
    let parse_input_file_result = interpreter::parse_input_file(args.file);
    match parse_input_file_result {
        Ok(valid_play_text) => println!("{:?}", valid_play_text),
        Err(err) => println!("{}", err)
    };
    Ok(())
}
