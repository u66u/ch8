mod utils;
mod memory;
mod cpu;
mod run;

use cpu::C8Kernel;
use run::{ Args, ExitKind };
use std::process::exit;

fn main() {
    let args: Args = argh::from_env();
    match C8Kernel::run_from_args(args) {
        Ok(kind) =>
            match kind {
                ExitKind::Natural => {
                    exit(0);
                }
                ExitKind::Forced => {
                    exit(130);
                }
            }
        Err(e) => {
            println!("An unexpected error occurred: {}", e);
            exit(1);
        }
    }
}
