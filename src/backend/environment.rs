use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;
use koopa::ir::{Function, Program, Value, ValueKind};
use crate::backend::register::{get_register_map, to_string, Register};

pub struct Environment<'a> {
    pub program: &'a Program,
    register_map: HashMap<Value, Register>,
    var_pos: HashMap<Value, usize>,
    global_var: HashMap<Value, String>,
    pub global_symbol: HashSet<Value>,
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
            global_symbol: HashSet::new(),
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
        let (register_map, max_reg_num) = get_register_map(self, self.program, func);
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
    pub fn get_offset(&mut self, base: &str, offset: i32, temp: &str) -> String {
        if offset > 2047 || offset < -2048 {
            self.output.write_all(format!("  li {}, {}\n", temp, offset).as_bytes()).unwrap();
            self.output.write_all(format!("  add {}, {}, {}\n", temp, base, temp).as_bytes()).unwrap();
            format!("0({})", temp)
        } else {
            format!("{}({})", offset, base)
        }
    }
    pub fn get_offset_inplace(&mut self, base: &str, offset: i32, temp: &str) -> String {
        if offset > 2047 || offset < -2048 {
            self.output.write_all(format!("  li {}, {}\n", temp, offset).as_bytes()).unwrap();
            self.output.write_all(format!("  add {}, {}, {}\n", base, base, temp).as_bytes()).unwrap();
            format!("0({})", base)
        } else {
            format!("{}({})", offset, base)
        }
    }
    pub fn get_reg_or_integer(&mut self, value: Value) -> Option<Register> {
        let value_data = self.program.func(self.cur_func.unwrap()).dfg().value(value);
        if let ValueKind::Integer(integer) = value_data.kind() {
            let reg = self.register_map.get(&value).unwrap();
            self.output.write_all(format!("  li {}, {}\n", to_string(*reg), integer.value()).as_bytes()).unwrap();
            return Some(*reg);
        }
        if let Some(reg) = self.register_map.get(&value) {
            return Some(*reg);
        }
        None
    }
    pub fn get_reg_with_load(&mut self, value: Value, temp_reg: Register) -> Option<Register> {
        if let Some(reg) = self.get_reg_or_integer(value) {
            return Some(reg);
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
    pub fn get_pos_(&mut self, value: Value, temp_reg: &str) -> Option<(String, usize)> {
        if let Some(reg) = self.register_map.get(&value) {
            return Some((to_string(*reg), 0));
        }
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(("sp".to_string(), *pos));
        }
        if let Some(name) = self.global_var.get(&value) {
            self.output.write_all(format!("la {}, {}\n", temp_reg, name).as_bytes()).unwrap();
            return Some((temp_reg.to_string(), 0));
        }
        None
    }

    pub fn get_pos(&mut self, value: Value, temp_reg: &str) -> Option<String> {
        if let Some(reg) = self.register_map.get(&value) {
            return Some(format!("0({})", to_string(*reg)));
        }
        if let Some(pos) = self.var_pos.get(&value) {
            let temp = self.get_offset("sp", *pos as i32, temp_reg);
            return Some(format!("{}", temp));
        }
        if let Some(name) = self.global_var.get(&value) {
            self.output.write_all(format!("la {}, {}\n", temp_reg, name).as_bytes()).unwrap();
            return Some(format!("0({})", temp_reg));
        }
        None
    }

}