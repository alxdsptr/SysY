use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use koopa::ir::{Function, Program, Value};
use crate::backend::register::{from_string, get_register_map, to_string, Register};

pub struct Environment<'a> {
    pub program: &'a Program,
    register_map: HashMap<Value, Register>,
    var_pos: HashMap<Value, usize>,
    global_var: HashMap<Value, String>,
    pub output: &'a mut File,
    pub cur_func: Option<Function>,
    pub cur_pos: usize,
    pub stack_size: usize,
    pub max_reg_num: u32,

}
// pub enum PosType {
//     SP(usize),
//     Reg
// }
impl Environment<'_> {
    pub fn new<'a>(program: &'a Program, output: &'a mut File) -> Environment<'a> {
        Environment {
            program,
            register_map: HashMap::new(),
            var_pos: HashMap::new(),
            global_var: HashMap::new(),
            output,
            cur_func: None,
            cur_pos: 0,
            stack_size: 0,
            max_reg_num: 0,
        }
    }
    pub fn enter_new_func(&mut self, func: Function) {
        self.cur_func = Some(func);
        self.var_pos.clear();
        let (register_map, max_reg_num) = get_register_map(self.program, func);
        self.register_map = register_map;
        self.max_reg_num = max_reg_num;
    }
    pub fn insert_global_variable(&mut self, name: Value, value: &str) {
        self.global_var.insert(name.clone(), value.to_string());
    }
    pub fn insert_stack_variable(&mut self, name: Value, offset: usize) {
        self.var_pos.insert(name, offset);
    }
    pub fn get_register(&self, value: Value) -> Option<Register> {
        self.register_map.get(&value).cloned()
    }
    pub fn get_reg_with_load(&mut self, value: Value, temp_reg: Register) -> Option<Register> {
        if let Some(reg) = self.register_map.get(&value) {
            return Some(*reg);
        }
        if let Some(name) = self.global_var.get(&value) {
            let str = to_string(temp_reg);
            self.output.write_all(format!("  la {}, {}\n", str, name).as_bytes()).unwrap();
            self.output.write_all(format!("  lw {}, 0({})\n", str, str).as_bytes()).unwrap();
            return Some(temp_reg);
        }
        None
    }
    pub fn get_symbol_pos(&mut self, value: Value, temp_reg: &str) -> Option<(String, usize)> {
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(("sp".to_string(), *pos));
        }
        if let Some(name) = self.global_var.get(&value) {
            self.output.write_all(format!("  la {}, {}\n", temp_reg, name).as_bytes()).unwrap();
            return Some((temp_reg.to_string(), 0));
        }
        None
    }

    pub fn get_pos(&mut self, value: Value, temp_reg: &str) -> Option<String> {
        if let Some(reg) = self.register_map.get(&value) {

            return Some(format!("0({})", reg.to_string()));
        }
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(format!("{}(sp)", pos));
        }
        if let Some(name) = self.global_var.get(&value) {
            self.output.write_all(format!("la {}, {}\n", temp_reg, name).as_bytes()).unwrap();
            return Some(format!("0({})", temp_reg));
        }
        None
    }

}

/*
pub struct SymbolTable {
    pub symbols: HashMap<Value, SymbolTableEntry>,
}

#[derive(Debug, Clone)]
pub enum SymbolTableEntry {
    Register(Register),
    Stack(usize),
    Global(String),
}*/