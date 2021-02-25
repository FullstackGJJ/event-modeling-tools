use quicli::prelude::*;
use std::fs;
use structopt::StructOpt;


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
    let lines = fs::read_to_string(&args.file)?;
    let lines = lines.lines();
    lines.for_each(|line| println!("{}", line));
    Ok(())
}
