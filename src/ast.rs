use std::cell::RefCell;
use std::iter::Product;
use std::rc::Rc;
use koopa::ir;
use koopa::ir::*;
use koopa::ir::builder_traits::*;
use crate::environment::Environment;
use crate::sym_table::SymbolTable;

#[derive(Debug)]
pub struct CompUnit {
    pub comp_unit: Rc<Option<CompUnit>>,
    pub func_def: Rc<FuncDef>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub params: Rc<Option<FuncFParams>>,
    pub ident: String,
    pub block: Rc<Block>,
}
impl FuncDef {
    pub fn get_param(&self) -> Vec<(Option<String>, Type)>{
        match &*self.params {
            Some(params) => {
                params.params.iter().map(|param| {
                    (Some(param.ident.clone()), param.btype.to_type())
                }).collect()
            },
            None => vec![],
        }
    }
}
#[derive(Debug)]
pub struct FuncFParams {
    pub params: Rc<Vec<FuncFParam>>,
}
impl FuncFParams {
    pub fn add_params(&self, env: &mut Environment) {
        let func = env.program.func_mut(env.cur_func.unwrap());
        for i in 0..self.params.len() {
            let val = func.params()[i];
            let name = self.params[i].ident.clone();
            env.sym_table.borrow_mut().insert_var(name, val);
        }
    }
}
#[derive(Debug)]
pub struct FuncFParam {
    pub btype: BType,
    pub ident: String,
}

#[derive(Debug)]
pub enum FuncType {
    Void,
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub items: Rc<Vec<BlockItem>>,
}
#[derive(Debug)]
pub enum BlockItem {
    Stmt(Rc<Stmt>),
    Decl(Rc<Decl>),
}

#[derive(Debug)]
pub enum Stmt {
    Return(Rc<Option<Exp>>),
    Assign(LVal, Rc<Exp>),
    Exp(Rc<Option<Exp>>),
    Block(Rc<Block>),
    If(Rc<Exp>, Rc<Stmt>, Option<Rc<Stmt>>),
    While(Rc<Exp>, Rc<Stmt>),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum Exp {
    Num(Number),
    LVal(LVal),
    UnaryExp(UnaryOp, Rc<Exp>),
    BinaryExp(BinaryOp, Rc<Exp>, Rc<Exp>),
    Call(String, Rc<Option<Vec<Exp>>>),
}
#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}
#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
    Land,
    Lor
}
impl BinaryOp {
    pub fn need_short_circuit(&self) -> bool {
        match self {
            BinaryOp::Land | BinaryOp::Lor => true,
            _ => false,
        }
    }
    pub fn to_koopa_op(&self) -> ir::BinaryOp {
        match self {
            BinaryOp::Add => ir::BinaryOp::Add,
            BinaryOp::Sub => ir::BinaryOp::Sub,
            BinaryOp::Mul => ir::BinaryOp::Mul,
            BinaryOp::Div => ir::BinaryOp::Div,
            BinaryOp::Mod => ir::BinaryOp::Mod,
            BinaryOp::Eq => ir::BinaryOp::Eq,
            BinaryOp::Neq => ir::BinaryOp::NotEq,
            BinaryOp::Gt => ir::BinaryOp::Gt,
            BinaryOp::Ge => ir::BinaryOp::Ge,
            BinaryOp::Lt => ir::BinaryOp::Lt,
            BinaryOp::Le => ir::BinaryOp::Le,
            BinaryOp::Land | BinaryOp::Lor => panic!("Logical operations should not be converted to koopa IR directly"),
        }
    }
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(Rc<ConstDecl>),
    VarDecl(Rc<VarDecl>),
}
#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub const_defs: Rc<Vec<ConstDef>>,
}
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: Rc<ConstInitVal>,
}
#[derive(Debug)]
pub enum ConstInitVal {
    ConstExp(Rc<Exp>)
}


#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Rc<Vec<VarDef>>,
}
#[derive(Debug)]
pub enum VarDef {
    Def(String),
    Init(String, Rc<InitVal>),
}
#[derive(Debug)]
pub enum InitVal {
    Exp(Rc<Exp>),
}
#[derive(Debug)]
pub enum BType {
    Int,
}
impl BType {
    pub fn to_type(&self) -> Type{
        match self {
            BType::Int => {
                Type::get_i32()
            }
        }
    }
}
pub type Number = i32;