use std::fmt::format;
use std::fs::File;
use std::io::Write;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value};
use koopa::ir::entities::ValueData;
use koopa::ir::ValueKind;
use koopa::ir::BinaryOp;
use crate::backend::environment::Environment;
use crate::backend::register::to_string;

trait CodeGen {
    fn code_gen(&self, env: &mut Environment);
}

impl CodeGen for Program {
    fn code_gen(&self, env: &mut Environment) {
        // Implement the code generation logic for the Program
        // This could involve iterating over the functions and generating code for each
        for (_, func) in self.funcs() {
            func.code_gen(env);
        }
    }
}
fn compute_stack_size(func: &FunctionData, env: &mut Environment) -> usize {
    let mut stack_size = 0;
    for (bb, _) in func.layout().bbs() {
        for (inst, _) in bb.insts() {
            let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(inst);
            match value_data.kind() {
                ValueKind::Alloc(alloc) => {
                    let size = value_data.ty().size();
                    stack_size += size;
                },
                _ => {}
            }
        }
    }
    let param_count = func.params().len();
    if param_count > 8 {
        stack_size -= (param_count - 8) * 4; // these params are stored in the stack of the caller
    }
    stack_size += 8; // return address and sp
    // align stack to 16 bytes
    stack_size = stack_size & (!0xF);
    stack_size
}
impl CodeGen for FunctionData {
    fn code_gen(&self, env: &mut Environment) {
        env.output.write_all(format!("{}:\n", self.name()).as_bytes()).unwrap();

        // store ra and sp to stack
        env.output.write_all("sw ra, -4(sp)\n".as_bytes()).unwrap();
        env.output.write_all("sw sp, -8(sp)\n".as_bytes()).unwrap();

        let stack_size = compute_stack_size(self, env);
        env.output.write_all(format!("addi sp, sp, -{}\n", stack_size).as_bytes()).unwrap();
        env.stack_size = stack_size;
        env.cur_pos = 8;
        let entry_bb = self.layout().entry_bb().unwrap();
        {
            let bb_data = env.program.func(env.cur_func.unwrap()).dfg().bb(entry_bb);
            env.output.write_all(format!("{}:\n", bb_data.name().as_ref().unwrap()).as_bytes()).unwrap();
        }
        let bb_node = env.program.func(env.cur_func.unwrap()).layout().bbs().node(&entry_bb).unwrap();
        let param_count = self.params().len();
        let mut cursor = bb_node.insts().cursor_front();
        for i in 0..param_count {
            let inst = cursor.key().unwrap();
            let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*inst);
            assert!(matches!(value_data.kind(), ValueKind::Alloc(_)));
            let offset = if i < 8 {
                env.cur_pos -= 4;
                env.stack_size - env.cur_pos
            } else {
                env.stack_size + 4 * (i - 8)
            };
            env.var_pos.insert(*inst, offset);

            cursor.move_next();
            let inst = cursor.key().unwrap();
            let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*inst);
            assert!(matches!(value_data.kind(), ValueKind::Store(_)));
            if i < 8 {
                // argument in ai register, store in pos offset(sp)
                env.output.write_all(format!("sw a{}, {}(sp)\n", i, offset).as_bytes()).unwrap();
            }
        }
        while let Some(inst) = cursor.key() {
            inst.code_gen(env);
            cursor.move_next();
        }
        for (bb, _) in self.layout().bbs().iter().skip(1) {
            bb.code_gen(env);
        }
    }
}

impl CodeGen for BasicBlock {
    fn code_gen(&self, env: &mut Environment) {
        {
            let bb_data = env.program.func(env.cur_func.unwrap()).dfg().bb(*self);
            env.output.write_all(format!("{}:\n", bb_data.name().as_ref().unwrap()).as_bytes()).unwrap();
        }
        let bb_node = env.program.func(env.cur_func.unwrap()).layout().bbs().node(self).unwrap();
        for (inst, _) in bb_node.insts() {
            inst.code_gen(env);
        }
    }
}

impl CodeGen for Value{
    fn code_gen(&self, env: &mut Environment) {
        let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*self);
        let str = match value_data.kind() {
            ValueKind::Binary(binary) => {
                let rs1 = to_string(*env.register_map.get(&binary.lhs()).unwrap());
                let rs2 = to_string(*env.register_map.get(&binary.rhs()).unwrap());
                let rd = to_string(*env.register_map.get(self).unwrap());

                match binary.op() {
                    BinaryOp::Add => format!("add {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Sub => format!("sub {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mul => format!("mul {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Div => format!("div {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mod => format!("rem {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Gt => format!("sgt {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Lt => format!("slt {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Eq => format!("sub {}, {}, {}\nseqz {}, {}", rd, rs1, rs2, rd, rd),
                    BinaryOp::NotEq => format!("sub {}, {}, {}\nsnez {}, {}", rd, rs1, rs2, rd, rd),
                    BinaryOp::Ge => format!("slt {}, {}, {}\nxori {}, {}, {}", rd, rs1, rs2, rd, rd, 1),
                    BinaryOp::Le => format!("sgt {}, {}, {}\nxori {}, {}, {}", rd, rs1, rs2, rd, rd, 1),
                    _ => {
                        panic!("Unsupported binary operation: {:?}", binary.op());
                    }
                }
            },
            ValueKind::Jump(jump) => {
                let target = jump.target();
                let target_name = env.program.func(env.cur_func.unwrap()).dfg().bb(target).name().as_ref().unwrap();
                format!("j {}\n", target_name)
            }
            ValueKind::Alloc(_) => {
                // empty string
                String::new()
            },
            ValueKind::Load(load) => {
                let rd = to_string(*env.register_map.get(self).unwrap());
                let src = match env.register_map.get(&load.src()) {
                    Some(rs) => {
                        to_string(*rs)
                    },
                    None => {
                        let offset = env.var_pos.get(&load.src()).unwrap();
                        format!("{}(sp)", offset)
                    }
                };
                format!("lw {}, {}\n", rd, src)
            },
            ValueKind::GetElemPtr(ptr) => {
                String::new()
            }
            _ => {
                panic!("Unsupported value kind");
            }
        };
        env.output.write_all(str.as_bytes()).unwrap();
    }
}