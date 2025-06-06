use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use koopa::ir::{Function, Type, Value};
use crate::ast::{BType, Number};
use crate::environment::FrontendError;

#[derive(Clone, Debug)]
pub enum SymbolEntry {
    Const(Number),
    Var(Value),
    Func(Function),
    Array(Value, Rc<Vec<i32>>, bool), // Value, dimensions, is_const
    ArrayPtr(Value, Rc<Vec<i32>>),
    StructDecl(Rc<HashMap<String, (BType, Vec<i32>)>>),
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
    pub fn insert_struct_decl(
        &mut self,
        name: String,
        struct_decl: HashMap<String, (BType, Vec<i32>)>,
    ) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        self.symbols.insert(name, SymbolEntry::StructDecl(struct_decl.into()));
        Ok(())
    }

    pub fn insert_var(&mut self, name: String, value: Value, is_param: bool) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        if is_param {
            self.symbols.insert(name, SymbolEntry::FuncParam(value));
        } else{
            self.symbols.insert(name, SymbolEntry::Var(value));
        }
        Ok(())
    }
    pub fn insert_const(&mut self, name: String, value: Number) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        self.symbols.insert(name, SymbolEntry::Const(value));
        Ok(())
    }
    pub fn insert_func(&mut self, name: String, func: Function) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        self.symbols.insert(name, SymbolEntry::Func(func));
        Ok(())
    }
    pub fn insert_array_ptr(
        &mut self,
        name: String,
        value: Value,
        dimensions: Rc<Vec<i32>>,
    ) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        self.symbols.insert(name, SymbolEntry::ArrayPtr(value, dimensions));
        Ok(())
    }
    pub fn insert_array(
        &mut self,
        name: String,
        value: Value,
        dimensions: Rc<Vec<i32>>,
        ty: Type,
        is_const: bool,
    ) -> Result<(), FrontendError> {
        if self.symbols.contains_key(&name) {
            return Err(FrontendError::Redefinition(name));
        }
        self.symbols.insert(name, SymbolEntry::Array(value, dimensions, is_const));
        Ok(())
    }
    pub fn is_global(&self) -> bool {
        self.parent.is_none()
    }

    pub fn get(&self, name: &str) -> Option<SymbolEntry> {
        if let Some(value) = self.symbols.get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }
}