use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use koopa::ir::Value;

pub struct SymTable {
    table: HashMap<String, Value>,
    parent: Option<Rc<RefCell<SymTable>>>,

}
impl SymTable {
    pub fn new() -> Self {
        SymTable {
            table: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<SymTable>>) -> Self {
        SymTable {
            table: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn insert(&mut self, name: String, value: Value) {
        self.table.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.table.get(name) {
            Some(*value)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }
}