use std::collections::HashMap;
use crate::backend::register::Register;

pub struct RiscvProgram {
    text: TextSection,
}
pub struct TextSection {
    funcs: Vec<Function>,
}
pub struct Function {
    sections: Vec<String>,
    section_data: HashMap<String, Vec<Inst>>
}
pub enum RiscVBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Sgt,
    Slt,
    Xor
}
pub enum Inst {
    Binary(RiscVBinaryOp, Register, Register, Register),
    BinaryImm(RiscVBinaryOp, Register, Register, i32),
    Seqz(Register, Register),
    Li(Register, i32),
    Mv(Register, Register),
    Sw(Register, Register, i32),
    Lw(Register, Register, i32),
}