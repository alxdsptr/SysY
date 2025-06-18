use std::collections::HashMap;
use std::fs::File;
use koopa::ir::{Function, Program, Value};
use crate::backend::register::Register;

pub struct Environment<'a> {
    pub program: &'a Program,
    pub register_map: HashMap<Value, Register>,
    pub var_pos: HashMap<Value, usize>,
    pub output: &'a File,
    pub cur_func: Option<Function>,
    pub cur_pos: usize,
    pub stack_size: usize

}
