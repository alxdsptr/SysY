use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use koopa::ir;
use koopa::ir::*;
use crate::environment::{Environment, FrontendError};
use crate::ir_gen::convert_dim;
use crate::sym_table::{SymbolEntry};

#[derive(Debug)]
pub struct CompUnit {
    pub items: Rc<Vec<Comp>>,
}

#[derive(Debug)]
pub enum Comp{
    FuncDef(Rc<FuncDef>),
    Decl(Rc<Decl>),
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
                    (Some(format!("@{}", param.ident)), param.to_type())
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
    pub fn add_params(&self, env: &mut Environment) -> Result<(), FrontendError> {
        let mut params_vals = Vec::new();
        {
            let func = env.program.func_mut(env.cur_func.unwrap());
            for param in func.params() {
                params_vals.push(*param);
            }
        }
        for i in 0..self.params.len() {
            let val = params_vals[i];
            let name = self.params[i].ident.clone();
            match self.params[i].array_size.as_ref() {
                Some(arr) => {
                    let (array_size, _) = convert_dim(env, arr)?;
                    env.sym_table.borrow_mut().insert_array_ptr(name, val, array_size.into())?;
                },
                None => {
                    env.sym_table.borrow_mut().insert_var(name, val, true)?;
                }
            }
        }
        Ok(())
    }
}
#[derive(Debug)]
pub struct FuncFParam {
    pub btype: BType,
    pub ident: String,
    pub array_size: Option<Vec<Exp>>,
}
impl FuncFParam {
    pub fn to_type(&self) -> Type {
        match self.array_size {
            Some(_) => {
                Type::get_pointer(self.btype.to_type())
            },
            None => self.btype.to_type()
        }
    }
}

#[derive(Debug)]
pub enum FuncType {
    Void,
    Int,
}
impl FuncType {
    pub fn from_btype(btype: BType) -> Self {
        match btype {
            BType::Int => FuncType::Int,
        }
    }
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
impl Exp {
    pub fn eval_const(&self, env: &mut Environment) -> Result<Number, FrontendError> {
        match self {
            Exp::Num(n) => Ok(*n),
            Exp::LVal(lval) => {
                if let Some(entry) = env.sym_table.borrow().get(&lval.ident) {
                    if let SymbolEntry::Const(value) = entry {
                        Ok(value)
                    } else {
                        Err(FrontendError::EvalNonConstExpr)
                    }
                } else {
                    Err(FrontendError::UndefinedVariable(lval.ident.clone()))
                }
            },
            Exp::UnaryExp(op, exp) => {
                let value = exp.eval_const(env)?;
                match op {
                    UnaryOp::Pos => Ok(value),
                    UnaryOp::Neg => Ok(-value),
                    UnaryOp::Not => Ok(!value),
                }
            },
            Exp::BinaryExp(op, left, right) => {
                let left_value = left.eval_const(env)?;
                let right_value = right.eval_const(env)?;
                match op {
                    BinaryOp::Add => Ok(left_value + right_value),
                    BinaryOp::Sub => Ok(left_value - right_value),
                    BinaryOp::Mul => Ok(left_value * right_value),
                    BinaryOp::Div => if right_value == 0 { Err(FrontendError::DivisionByZero) } else { Ok(left_value / right_value) },
                    BinaryOp::Mod => if right_value == 0 { Err(FrontendError::DivisionByZero) } else { Ok(left_value % right_value) },
                    BinaryOp::Eq => Ok((left_value == right_value) as i32),
                    BinaryOp::Neq => Ok((left_value != right_value) as i32),
                    BinaryOp::Gt => Ok((left_value > right_value) as i32),
                    BinaryOp::Ge => Ok((left_value >= right_value) as i32),
                    BinaryOp::Lt => Ok((left_value < right_value) as i32),
                    BinaryOp::Le => Ok((left_value <= right_value) as i32),
                    BinaryOp::Land => Ok((left_value != 0 && right_value != 0) as i32),
                    BinaryOp::Lor => Ok((left_value != 0 || right_value != 0) as i32),
                }
            },
            Exp::Call(_, _) => Err(FrontendError::EvalNonConstExpr), // Function calls cannot be evaluated at compile time
        }
    }
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
    pub array_index: Vec<Exp>,
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
    pub array_size: Vec<Exp>,
    pub const_init_val: Rc<InitVal>,
}
// #[derive(Debug)]
// pub enum ConstInitVal {
//     ConstExp(Rc<Exp>),
//     Array(Option<Rc<Vec<ConstInitVal>>>),
// }


#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Rc<Vec<VarDef>>,
}
#[derive(Debug)]
pub enum VarDef {
    Def(String, Vec<Exp>),
    Init(String, Vec<Exp>, Rc<InitVal>),
}
#[derive(Debug)]
pub enum InitVal {
    Exp(Rc<Exp>),
    Array(Rc<RefCell<VecDeque<InitVal>>>),
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
    pub fn from_func_type(func_type: FuncType) -> BType {
        match func_type {
            FuncType::Int => BType::Int,
            FuncType::Void => panic!("Cannot convert void function type to BType"),
        }
    }
}
pub type Number = i32;