use std::collections::HashMap;
use crate::backend::asm::{Reg, Inst, REG_NUM, RiscVBinaryOp};

pub struct Peekhole {
}

impl Peekhole {
    pub fn run_on(insts: &mut Vec<Inst>) {
        let mut imm_reg: Vec<Option<i32>> = vec![None; REG_NUM as usize];
        let mut delete_insts = Vec::new();
        for (i, inst) in insts.iter().enumerate() {
            match inst {
                Inst::Li(rd, imm) => {
                    if let Some(old_num) = imm_reg[rd.0 as usize] {
                        if old_num == *imm {
                            delete_insts.push(i);
                        }
                    }
                    imm_reg[rd.0 as usize] = Some(*imm);
                }
                Inst::Mv(rd, _) => {
                    imm_reg[rd.0 as usize] = None;
                }
                Inst::Binary(_, rd, _, _) => {
                    imm_reg[rd.0 as usize] = None;
                }
                Inst::BinaryImm(op, rd, rs, imm) => {
                    if *op == RiscVBinaryOp::Add && *imm == 0 && *rd == *rs {
                        delete_insts.push(i);
                    } else {
                        imm_reg[rd.0 as usize] = None;
                    }
                }
                Inst::Seqz(rd, _) => {
                    imm_reg[rd.0 as usize] = None;
                }, 
                Inst::Snez(rd, _) => {
                    imm_reg[rd.0 as usize] = None;
                }
                Inst::Lw(rd, _, _) => {
                    imm_reg[rd.0 as usize] = None;
                }
                Inst::La(rd, _) => {
                    imm_reg[rd.0 as usize] = None;
                }
                Inst::Call(_) | Inst::Jump(_) | Inst::Bnez(_, _) => {
                    for j in 0..REG_NUM {
                        imm_reg[j as usize] = None;
                    }
                }
                _ => {}
            }
        };
        // Remove the instructions that are marked for deletion
        for &i in delete_insts.iter().rev() {
            insts.remove(i);
        }

    }
}