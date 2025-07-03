use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;
use koopa::ir::{BasicBlock, Function, Program, Value, ValueKind};
use crate::backend::asm::{Inst, Reg, RiscVBinaryOp};
use crate::backend::register::{get_register_map, to_string, Register, X0, A0};

pub struct Environment<'a> {
    pub program: &'a Program,
    pub register_map: HashMap<Value, Register>,
    var_pos: HashMap<Value, usize>,
    global_var: HashMap<Value, String>,
    pub global_symbol: HashSet<Value>,
    pub output: &'a mut File,
    pub cur_func: Option<Function>,
    pub cur_pos: usize,
    pub stack_size: usize,
    pub active_at_entry: HashMap<BasicBlock, HashSet<Value>>,
    pub active_map: HashMap<Value, HashSet<Value>>,

}

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
            active_at_entry: HashMap::new(),
            active_map: HashMap::new(),
        }
    }
    pub fn enter_new_func(&mut self, func: Function) {
        self.cur_func = Some(func);
        self.var_pos.clear();
        let mut hint = HashMap::new();
        let func_data = self.program.func(func);
        for (i, param) in func_data.params().iter().enumerate() {
            if i < 8 {
                hint.insert(*param, A0 + i as Register);
            }
        }
        for (_, bb_node) in func_data.layout().bbs() {
            for val in bb_node.insts().keys() {
                let value_data = self.program.func(func).dfg().value(*val);
                if let ValueKind::Call(call) = value_data.kind() {
                    for (i, arg) in call.args().iter().enumerate() {
                        if i < 8 {
                            hint.insert(*arg, A0 + i as Register);
                        }
                    }
                }
            }
        }
        let pre_defined = HashMap::new();
        let (register_map, active_at_entry, active_map) = get_register_map(self, self.program, func, &hint, pre_defined);
        self.register_map = register_map;
        self.active_at_entry = active_at_entry;
        self.active_map = active_map;
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
    pub fn get_offset(&mut self, base: &str, offset: i32, temp: &str, insts: &mut Vec<Inst>) -> (String, usize) {
        if offset > 2047 || offset < -2048 {
            insts.push(Inst::Li(Reg::from_string(temp), offset));
            insts.push(Inst::Binary(RiscVBinaryOp::Add, Reg::from_string(temp), Reg::from_string(base), Reg::from_string(temp)));
            (temp.to_string(), 0)
        } else {
            (base.to_string(), offset as usize)
        }
    }
    pub fn get_reg_or_integer(&mut self, value: Value, insts: &mut Vec<Inst>) -> Option<Register> {
        let value_data = self.program.func(self.cur_func.unwrap()).dfg().value(value);
        if let ValueKind::Integer(integer) = value_data.kind() {
            if integer.value() == 0 {
                return Some(X0);
            }
            let reg = self.register_map.get(&value).unwrap();
            insts.push(Inst::Li(Reg::from_string(&to_string(*reg)), integer.value()));
            return Some(*reg);
        }
        if let Some(reg) = self.register_map.get(&value) {
            return Some(*reg);
        }
        None
    }
    pub fn get_reg_with_load(&mut self, value: Value, temp_reg: Register, insts: &mut Vec<Inst>) -> Option<Register> {
        if let Some(reg) = self.get_reg_or_integer(value, insts) {
            return Some(reg);
        }
        if let Some(name) = self.global_var.get(&value) {
            let str = to_string(temp_reg);
            insts.push(Inst::La(Reg::from_string(&str), name.to_string()));
            insts.push(Inst::Lw(Reg::from_string(&str), Reg::from_string(&str), 0));
            return Some(temp_reg);
        }
        None
    }
    pub fn get_symbol_pos(&mut self, value: Value, temp_reg: &str, insts: &mut Vec<Inst>) -> Option<(String, usize)> {
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(("sp".to_string(), *pos));
        }
        if let Some(name) = self.global_var.get(&value) {
            insts.push(Inst::La(Reg::from_string(&temp_reg), name.to_string()));
            return Some((temp_reg.to_string(), 0));
        }
        None
    }
    pub fn get_pos_(&mut self, value: Value, temp_reg: &str, insts: &mut Vec<Inst>) -> Option<(String, usize)> {
        if let Some(reg) = self.register_map.get(&value) {
            return Some((to_string(*reg), 0));
        }
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(("sp".to_string(), *pos));
        }
        if let Some(name) = self.global_var.get(&value) {
            insts.push(Inst::La(Reg::from_string(&temp_reg), name.to_string()));
            return Some((temp_reg.to_string(), 0));
        }
        None
    }

    pub fn get_pos(&mut self, value: Value, temp_reg: &str, insts: &mut Vec<Inst>) -> Option<(String, usize)> {
        if let Some(reg) = self.register_map.get(&value) {
            return Some((to_string(*reg), 0));
        }
        if let Some(pos) = self.var_pos.get(&value) {
            return Some(self.get_offset("sp", *pos as i32, temp_reg, insts))
        }
        if let Some(name) = self.global_var.get(&value) {
            insts.push(Inst::La(Reg::from_string(&temp_reg), name.to_string()));
            return Some((temp_reg.to_string(), 0));
        }
        None
    }

}