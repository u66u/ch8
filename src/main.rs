mod utils;
mod memory;
mod cpu;
mod instructions;

use cpu::C8Kernel;
use instructions::{ Args, ExitType };
use std::process::exit;

fn main() {
    let args: Args = argh::from_env();
    match C8Kernel::run_from_args(args) {
        Ok(kind) =>
            match kind {
                ExitType::Natural => {
                    exit(0);
                }
                ExitType::Forced => {
                    exit(130);
                }
            }
        Err(e) => {
            println!("An unexpected error occurred: {}", e);
            exit(1);
        }
    }
}
