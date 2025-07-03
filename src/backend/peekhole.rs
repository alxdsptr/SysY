use std::collections::HashMap;
use crate::backend::asm::{Reg, Inst, RegNum};

pub struct Peekhole {
}

impl Peekhole {
    pub fn run_on(insts: &mut Vec<Inst>) -> Vec<Inst> {
        let mut imm_reg: Vec<Option<i32>> = vec![None; RegNum];
        let mut delete_insts = Vec::new();
        let mut change_insts: HashMap<usize, Inst> = HashMap::new();
        for (i, inst) in insts.iter().enumerate() {
            match inst {
                Inst::Li(rd, imm) => {
                    imm_reg[rd as usize] = Some(imm);
                }
                Inst::Mv(rd, rs) => {
                    if let Some(num) = imm_reg[rs as usize] {
                        change_insts.insert(i, Inst::Li(rd, num));
                    }
                }
                _ => {
                    new_insts.push(inst);
                }
            }
        }

    }
}