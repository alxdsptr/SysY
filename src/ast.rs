use std::iter::Product;
use std::rc::Rc;
use koopa::ir::*;
use koopa::ir::builder_traits::*;
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: Rc<FuncDef>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Rc<Block>,
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
    If(Rc<Exp>, Rc<Stmt>, Rc<Option<Stmt>>),
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
    LVal(LVal),
}
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
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
pub type Number = i32;