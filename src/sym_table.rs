use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use koopa::ir::{Function, Value};
use crate::ast::Number;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolEntry {
    Const(Number),
    Var(Value),
    Func(Function)

}
pub struct SymbolTable {
    symbols: HashMap<String, SymbolEntry>,
    parent: Option<Rc<RefCell<SymbolTable>>>,

}
impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: &Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            parent: Some(parent.clone()),
        }
    }

    pub fn insert_var(&mut self, name: String, value: Value) {
        self.symbols.insert(name, SymbolEntry::Var(value));
    }
    pub fn insert_const(&mut self, name: String, value: Number) {
        self.symbols.insert(name, SymbolEntry::Const(value));
    }
    pub fn insert_func(&mut self, name: String, func: Function) {
        self.symbols.insert(name, SymbolEntry::Func(func));
    }

    pub fn get(&self, name: &str) -> Option<SymbolEntry> {
        if let Some(value) = self.symbols.get(name) {
            Some(*value)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }
}