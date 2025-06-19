pub mod environment;
pub mod code_gen;
mod register;

use std::fs::File;
use koopa::ir::Program;
use crate::backend::code_gen::CodeGen;
use crate::backend::environment::Environment;

pub fn generate_asm(program: &Program, output: &mut File) {
    let mut env = Environment::new(program, output);
    program.code_gen(&mut env);
}