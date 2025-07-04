use std::cell::RefCell;
use std::rc::Rc;
use koopa::ir::{BasicBlock, Function, Program, BinaryOp, Value, Type, FunctionData};
use koopa::ir::builder::*;
use crate::frontend::label_gen::LabelGenerator;
use crate::frontend::sym_table::SymbolTable;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct WhileEnv {
    pub start: BasicBlock,
    pub next: BasicBlock,
}
pub struct Environment<'a>{
    pub program: &'a mut Program,
    pub cur_func: Option<Function>,
    pub cur_bb: Option<BasicBlock>,
    pub while_env: Option<WhileEnv>,
    pub label_gen: Rc<RefCell<LabelGenerator>>,
    pub sym_table: Rc<RefCell<SymbolTable>>,
}
impl Environment<'_> {
    pub fn new(program: &mut Program) -> Environment {
        Environment {
            program,
            cur_func: None,
            cur_bb: None,
            while_env: None,
            label_gen: Rc::new(RefCell::new(LabelGenerator::new())),
            sym_table: Rc::new(RefCell::new(SymbolTable::new())),
        }
    }
    // pub fn get_symbol_entry()
    pub fn create_block(&mut self, name: &str) -> BasicBlock{
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let name = self.label_gen.borrow_mut().get_label(name);
        let bb = func_data.dfg_mut().new_bb().basic_block(Some(name));
        func_data.layout_mut().bbs_mut().push_key_back(bb).unwrap();
        bb
    }
    pub fn enter_scope(&mut self) -> Rc<RefCell<SymbolTable>> {
        let old_table = self.sym_table.clone();
        let new_table = Rc::new(RefCell::new(SymbolTable::new_with_parent(&self.sym_table)));
        self.sym_table = new_table;
        old_table
    }
    pub fn exit_scope(&mut self, old_table: Rc<RefCell<SymbolTable>>) {
        self.sym_table = old_table;
    }
    pub fn add_decl(&mut self, name: &str, params_ty: Vec<Type>, ret_ty: Type) -> Result<(), FrontendError> {
        let function = self.program.new_func(
            FunctionData::new_decl(format!("@{}", name), params_ty.clone(), ret_ty.clone()));
        self.sym_table.borrow_mut().insert_func(name.to_string(), function)?;
        Ok(())
    }
    pub fn add_binary_inst(
        &mut self, op: BinaryOp, lhs: Value, rhs: Value,
    ) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().binary(op, lhs, rhs);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_jump(&mut self, target: BasicBlock) -> Value{
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().jump(target);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_alloc(&mut self, ty: koopa::ir::Type, name: &str) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().alloc(ty);
        func_data.dfg_mut().set_value_name(res, Some("@".to_string() + name));
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_global_alloc(&mut self, initializer: Value, name: String) -> Value {
        let res = self.program.new_value().global_alloc(initializer);
        self.program.set_value_name(res, Some(format!("@{}", name)));
        res
    }
    pub fn add_store(&mut self, value: Value, dest: Value) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().store(value, dest);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_load(&mut self, src: Value) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().load(src);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_getelemptr(&mut self, src: Value, index: Value) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().get_elem_ptr(src, index);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_get_ptr(&mut self, src: Value, index: Value) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().get_ptr(src, index);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_ret(&mut self, value: Option<Value>) -> Value {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().ret(value);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        // let new_bb = self.create_block("after_ret");
        // self.cur_bb = Some(new_bb);
        res
    }
    pub fn add_integer(&mut self, value: i32) -> Value {
        if self.is_global() {
            self.program.new_value().integer(value)
        } else {
            let func_data = self.program.func_mut(self.cur_func.unwrap());
            func_data.dfg_mut().new_value().integer(value)
        }
    }
    pub fn add_branch(&mut self, cond: Value, true_bb: BasicBlock, false_bb: BasicBlock) -> Value{
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        let res = func_data.dfg_mut().new_value().branch(cond, true_bb, false_bb);
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(res)
            .unwrap();
        res
    }
    pub fn add_aggregate(&mut self, values: Vec<Value>) -> Value {
        if self.is_global() {
            self.program.new_value().aggregate(values)
        } else {
            let func_data = self.program.func_mut(self.cur_func.unwrap());
            func_data.dfg_mut().new_value().aggregate(values)
        }
    }

    pub fn add_inst(&mut self, inst: Value) {
        let func_data = self.program.func_mut(self.cur_func.unwrap());
        func_data.layout_mut().bb_mut(self.cur_bb.unwrap().clone())
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }
    pub fn is_global(&self) -> bool {
        self.sym_table.borrow().is_global()
    }
}

#[derive(Debug)]
pub enum FrontendError {
    ContinueOutsideLoop,
    BreakOutsideLoop,
    EvalNonConstExpr,
    DivisionByZero,
    InvalidArrayDim,
    InvalidArrayInitializer,
    ArrayIndexMismatch(String),
    UndefinedVariable(String),
    UndefinedFunction(String),
    Redefinition(String),
    InvalidAssignment(String),
}

pub trait IRGen {
    type Output;
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError>;
}