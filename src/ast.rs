use std::iter::Product;
use std::rc::Rc;
use koopa::ir::*;
use koopa::ir::builder_traits::*;
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: Rc<FuncDef>,
}
impl CompUnit {
    pub fn generate_ir(&self, program: &mut Program) {
        self.func_def.generate_ir(program);
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Rc<Block>,
}
impl FuncDef {
    pub fn generate_ir(&self, program: &mut Program) {
        let func = program.new_func(FunctionData::new(
            self.ident.clone(),
            Vec::new(),
            match self.func_type {
                FuncType::Void => Type::get_unit(),
                FuncType::Int => Type::get_i32()
            },
        ));
        let func = program.func_mut(func);

    }
}

#[derive(Debug)]
pub enum FuncType {
    Void,
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Rc<Stmt>,
}

impl Block {
    pub fn generate_ir(&self, program: &mut Program) {
        // self.stmt.generate_ir(program);
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub exp : Rc<Exp>,
}

#[derive(Debug)]
pub struct Exp {
    pub l_or_exp: Rc<LOrExp>,
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(Rc<LAndExp>),
    Or(Rc<LOrExp>, Rc<LAndExp>),
}
#[derive(Debug)]
pub enum LAndExp {
    EqExp(Rc<EqExp>),
    And(Rc<LAndExp>, Rc<EqExp>),
}
#[derive(Debug)]
pub enum EqExp {
    RelExp(Rc<RelExp>),
    Eq(Rc<EqExp>, EqOp, Rc<RelExp>),
}
#[derive(Debug)]
pub enum EqOp {
    Eq,
    Neq,
}
#[derive(Debug)]
pub enum RelExp {
    AddExp(Rc<AddExp>),
    Rel(Rc<RelExp>, RelOp, Rc<AddExp>),
}
#[derive(Debug)]
pub enum RelOp {
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(Rc<MulExp>),
    Add(Rc<AddExp>, AddOp, Rc<MulExp>),
}
#[derive(Debug)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(Rc<UnaryExp>),
    Mul(Rc<MulExp>, MulOp, Rc<UnaryExp>),
}
#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Rc<PrimaryExp>),
    UnaryOp(UnaryOp, Rc<UnaryExp>),
}
#[derive(Debug)]
pub enum PrimaryExp {
    Parens(Rc<Exp>),
    Number(Number),
}
#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

pub type Number = i32;