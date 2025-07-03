use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use once_cell::sync::Lazy;
use crate::backend::peekhole::Peekhole;
// riscv 寄存器命名规则

static REG_MAP: Lazy<HashMap<&'static str, u32>> = Lazy::new(|| {
    HashMap::from([
        ("x0", 0), ("ra", 1), ("sp", 2), ("gp", 3),
        ("tp", 4), ("t0", 5), ("t1", 6), ("t2", 7),
        ("s0", 8), ("s1", 9), ("a0", 10), ("a1", 11),
        ("a2", 12), ("a3", 13), ("a4", 14), ("a5", 15),
        ("a6", 16), ("a7", 17), ("s2", 18), ("s3", 19),
        ("s4", 20), ("s5", 21), ("s6", 22), ("s7", 23),
        ("s8", 24), ("s9", 25), ("s10", 26), ("s11", 27),
        ("t3", 28), ("t4", 29), ("t5", 30), ("t6", 31),
    ])
});
pub(crate) const REG_NUM: i32 = 32;
static REG_NAME: Lazy<Vec<&str>> = Lazy::new(|| {
    vec![
        "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
        "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
        "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
        "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
    ]
});
#[derive(Copy, Clone)]
#[derive(PartialEq)]
pub struct Reg(pub u32);
impl Reg {
    pub fn from_string(s: &str) -> Reg {
        REG_MAP.get(s).map(|r| Reg(*r)).unwrap()
    }
}
impl From<Reg> for usize {
    fn from(reg: Reg) -> usize {
        reg.0 as usize
    }
}
impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", REG_NAME[self.0 as usize])
    }
}
pub struct RiscvProgram {
    text: TextSection,
}
pub struct TextSection {
    funcs: Vec<Function>,
}
impl TextSection {
    pub fn new(funcs: Vec<Function>) -> TextSection {
        TextSection {
            funcs,
        }
    }
    pub fn dump(&self, output: &mut File) {
        for func in &self.funcs {
            func.dump(output);
        }
    }
}
pub struct Function {
    name: String,
    sections: Vec<String>,
    section_data: HashMap<String, Vec<Inst>>
}
impl Function {
    pub fn new(name: &str) -> Function {
        Function {
            name: name.to_string(),
            sections: Vec::new(),
            section_data: HashMap::new(),
        }
    }
    pub fn add_section(&mut self, name: &str, data: Vec<Inst>) {
        self.sections.push(name.to_string());
        self.section_data.insert(name.to_string(), data);
    }
    pub fn dump(&self, output: &mut File) {
        output.write_all(format!("  .globl {}\n{}:\n", &self.name, &self.name).as_bytes()).unwrap();
        for section in &self.sections {
            output.write_all(format!("{}:\n", section).as_bytes()).unwrap();
            for inst in &self.section_data[section] {
                inst.dump(output);
            }
        }
    }
    pub fn optimize(&mut self) {
        for section in self.section_data.values_mut() {
            Peekhole::run_on(section);
        }
    }
}
#[derive(PartialEq)]
pub enum RiscVBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Sgt,
    Slt,
    Xor,
    Sll,
}
impl RiscVBinaryOp {
    pub fn to_imm_op(&self) -> String {
        match self {
            RiscVBinaryOp::Add => "addi".to_string(),
            RiscVBinaryOp::Xor => "xori".to_string(),
            _ => panic!("Unsupported operation for immediate: {}", self),
        }
    }
}
impl Display for RiscVBinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RiscVBinaryOp::Add => write!(f, "add"),
            RiscVBinaryOp::Sub => write!(f, "sub"),
            RiscVBinaryOp::Mul => write!(f, "mul"),
            RiscVBinaryOp::Div => write!(f, "div"),
            RiscVBinaryOp::Rem => write!(f, "rem"),
            RiscVBinaryOp::Sgt => write!(f, "sgt"),
            RiscVBinaryOp::Slt => write!(f, "slt"),
            RiscVBinaryOp::Xor => write!(f, "xor"),
            RiscVBinaryOp::Sll => write!(f, "sll"),
        }
    }
}
pub enum Inst {
    Binary(RiscVBinaryOp, Reg, Reg, Reg),
    BinaryImm(RiscVBinaryOp, Reg, Reg, i32),
    Seqz(Reg, Reg),
    Snez(Reg, Reg),
    Li(Reg, i32),
    Mv(Reg, Reg),
    Sw(Reg, Reg, i32),
    Lw(Reg, Reg, i32),
    La(Reg, String),
    Jump(String),
    Bnez(Reg, String),
    Call(String),
    Ret,
    Label(String),
}
impl Inst {
    pub fn dump(&self, output: &mut File) {
        match self {
            Inst::Binary(op, rd, rs1, rs2) => output.write_all(format!("  {} {}, {}, {}\n", op, rd, rs1, rs2).as_bytes()).unwrap(),
            Inst::BinaryImm(op, rd, rs1, imm) => output.write_all(format!("  {} {}, {}, {}\n", op.to_imm_op(), rd, rs1, imm).as_bytes()).unwrap(),
            Inst::Seqz(rd, rs) => output.write_all(format!("  seqz {}, {}\n", rd, rs).as_bytes()).unwrap(),
            Inst::Snez(rd, rs) => output.write_all(format!("  snez {}, {}\n", rd, rs).as_bytes()).unwrap(),
            Inst::Li(rd, imm) => output.write_all(format!("  li {}, {}\n", rd, imm).as_bytes()).unwrap(),
            Inst::Mv(rd, rs) => output.write_all(format!("  mv {}, {}\n", rd, rs).as_bytes()).unwrap(),
            Inst::Sw(rs1, rs2, off) => output.write_all(format!("  sw {}, {}({})\n", rs1, off, rs2).as_bytes()).unwrap(),
            Inst::Lw(rd, rs, off) => output.write_all(format!("  lw {}, {}({})\n", rd, off, rs).as_bytes()).unwrap(),
            Inst::Jump(label) => output.write_all(format!("  j {}\n", label).as_bytes()).unwrap(),
            Inst::Bnez(rs, label) => output.write_all(format!("  bnez {}, {}\n", rs, label).as_bytes()).unwrap(),
            Inst::Ret => output.write_all("  ret\n".as_bytes()).unwrap(),
            Inst::Call(func) => output.write_all(format!("  call {}\n", func).as_bytes()).unwrap(),
            Inst::La(rd, label) => output.write_all(format!("  la {}, {}\n", rd, label).as_bytes()).unwrap(),
            Inst::Label(label) => output.write_all(format!("{}:\n", label).as_bytes()).unwrap(),
        }
    }
}
