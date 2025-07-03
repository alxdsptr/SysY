use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::io::Write;
use std::sync::atomic::AtomicUsize;
use koopa::ir::{BasicBlock, FunctionData, Program, TypeKind, Value};
use koopa::ir::ValueKind;
use koopa::ir::BinaryOp;
use crate::backend::asm;
use crate::backend::environment::Environment;
use crate::backend::register::{to_string, Register, A0, FREE_REG, T5, T6};
use crate::backend::asm::{Reg, Inst, RiscVBinaryOp};

pub trait CodeGen {
    type Output;
    fn code_gen(&self, env: &mut Environment) -> Self::Output;
}

impl CodeGen for Program {
    type Output = ();
    fn code_gen(&self, env: &mut Environment) -> Self::Output {
        // data section
        env.output.write_all("  .data\n".as_bytes()).unwrap();
        for &val in self.inst_layout() {
            env.global_symbol.insert(val);
            let value_data = self.borrow_value(val);
            match value_data.kind() {
                ValueKind::GlobalAlloc(alloc) => {
                    let name = &value_data.name().as_ref().unwrap()[1..];
                    env.output.write_all(format!("  .globl {}\n{}:\n", name, name).as_bytes()).unwrap();
                    env.insert_global_variable(val, name);

                    let init = self.borrow_value(alloc.init());
                    match init.kind() {
                        ValueKind::Integer(num) => {
                            let num = num.value();
                            env.output.write_all(format!("  .word {}\n", num).as_bytes()).unwrap();
                        },
                        ValueKind::ZeroInit(_) => {
                            let size = match value_data.ty().kind() {
                                TypeKind::Pointer(ptr) => ptr.size(),
                                _ => unreachable!()
                            };
                            env.output.write_all(format!("  .zero {}\n", size).as_bytes()).unwrap();
                        },
                        ValueKind::Aggregate(aggregate) => {
                            for elem in aggregate.elems() {
                                let elem_value = self.borrow_value(*elem);
                                match elem_value.kind() {
                                    ValueKind::Integer(num) => {
                                        env.output.write_all(format!("  .word {}\n", num.value()).as_bytes()).unwrap();
                                    },
                                    _ => {unreachable!()}
                                }
                            }
                        },
                        _ => {
                            panic!("Unsupported global alloc init value: {:?}", init.kind());
                        }
                    }
                },
                _ => {}
            }

        }
        // function section
        env.output.write_all(".text\n".as_bytes()).unwrap();
        for (func, func_data) in self.funcs() {
            if func_data.layout().entry_bb().is_none() {
                continue;
            }
            env.enter_new_func(*func);
            let mut func = func_data.code_gen(env);
            func.optimize();
            func.dump(env.output);
        }
    }
}
fn compute_stack_size(func: &FunctionData, env: &mut Environment) -> usize {
    let mut stack_size = 0;
    let mut max_arg_num = 0;
    for (_, bb) in func.layout().bbs() {
        for (inst, _) in bb.insts() {
            let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*inst);
            match value_data.kind() {
                ValueKind::Alloc(_) => {
                    let size = match value_data.ty().kind() {
                        TypeKind::Pointer(ptr) => {
                            ptr.size()
                        },
                        _ => unreachable!()
                    };
                    stack_size += size;
                },
                ValueKind::Call(call) => {
                    let arg_count = call.args().len();
                    if arg_count > max_arg_num {
                        max_arg_num = arg_count;
                    }
                },
                _ => {}
            }
        }
    }
    if max_arg_num > 8 {
        stack_size += (max_arg_num - 8) * 4; // these args are stored in the stack of the callee
    }
    stack_size += (25 * 4) as usize; // reserve space for registers
    stack_size += 8; // return address and sp
    // align stack to 16 bytes
    stack_size = (stack_size + 15) & (!0xF);
    stack_size
}
fn get_addi(rd: &str, rs: &str, imm: i32, temp: &str) -> Vec<Inst> {
    if imm > 2047 || imm < -2048 {
        vec![Inst::Li(Reg::from_string(temp), imm), Inst::Binary(RiscVBinaryOp::Add, Reg::from_string(rd), Reg::from_string(temp), Reg::from_string(rs))]
    } else {
        vec![Inst::BinaryImm(RiscVBinaryOp::Add, Reg::from_string(rd), Reg::from_string(rs), imm)]
    }
}
fn get_used_reg(reg_map: &HashMap<Value, Register>, active_vals: &HashSet<Value>) -> Vec<bool> {
    let mut used_regs = vec![false; FREE_REG];
    for val in active_vals {
        if let Some(&reg) = reg_map.get(&val) {
            used_regs[reg as usize] = true;
        }
    }
    used_regs
}

fn get_all_used_reg(reg_map: &HashMap<Value, Register>) -> Vec<bool> {
    let mut used_regs = vec![false; FREE_REG];
    for reg in reg_map.values() {
        used_regs[*reg as usize] = true;
    }
    used_regs
}
impl CodeGen for FunctionData {
    type Output = asm::Function;
    fn code_gen(&self, env: &mut Environment) -> Self::Output {
        let mut func = asm::Function::new(&self.name()[1..]);
        let label = format!("{}_entry", &self.name()[1..]);
        let mut insts = Vec::new();
        // println!("{}, max reg num: {}", self.name(), env.max_reg_num);

        // store ra and sp to stack
        insts.push(Inst::Sw(Reg::from_string("ra"), Reg::from_string("sp"), -4));
        insts.push(Inst::Sw(Reg::from_string("sp"), Reg::from_string("sp"), -8));

        let used_regs = get_all_used_reg(&env.register_map);
        let mut max_reg = 0;
        for i in 0..12 {
            let reg = to_string(i);
            if used_regs[i as usize] {
                insts.push(Inst::Sw(Reg::from_string(&reg), Reg::from_string("sp"), -((12 + i * 4) as i32)));
                max_reg = max(max_reg, i);
            }
        }
        max_reg += 1;

        let stack_size = compute_stack_size(self, env);
        env.stack_size = stack_size;

        let param_count = self.params().len();
        for i in 0..param_count {
            let reg = env.get_register(self.params()[i]).unwrap();
            if  i < 8 {
                if reg != A0 + i as Register {
                    insts.push(Inst::Mv(Reg::from_string(&to_string(reg)), Reg::from_string(&format!("a{}", i))));
                }
            } else {
                let offset = 4 * (i - 8);
                insts.push(Inst::Lw(Reg::from_string(&to_string(reg)), Reg::from_string("sp"), offset as i32));
            }
        }
        // env.output.write_all(get_addi("sp", "sp", -(stack_size as i32), "t5").as_bytes()).unwrap();
        insts.extend(get_addi("sp", "sp", -(stack_size as i32), "t5"));
        env.cur_pos = (8 + max_reg * 4) as usize; // 8 for ra and sp, 4 for each register
        func.add_section(label.as_str(), insts);
        
        for (bb, _) in self.layout().bbs().iter() {
            let insts = bb.code_gen(env);
            let bb_name = &self.dfg().bb(*bb).name().as_ref().unwrap()[1..];
            func.add_section(bb_name, insts);
        }
        func
    }
}

impl CodeGen for BasicBlock {
    type Output = Vec<Inst>;
    fn code_gen(&self, env: &mut Environment) -> Self::Output {
        let bb_node = env.program.func(env.cur_func.unwrap()).layout().bbs().node(self).unwrap();
        let mut insts = Vec::new();
        for (inst, _) in bb_node.insts() {
            insts.extend(inst.code_gen(env));
        }
        insts
    }
}
#[derive(Copy, Clone)]
enum RegOrInt {
    Reg(Register),
    Int(i32),
}
fn process_jump(args: &[Value], block_params: &[Value], env: &mut Environment, temp_reg: &str) -> Vec<Inst> {
    let mut args : Vec<RegOrInt> = args.iter().map(|v| {
        let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*v);
        match value_data.kind() {
            ValueKind::Integer(num) => RegOrInt::Int(num.value()),
            _ => {
                let reg = env.get_register(*v).unwrap();
                RegOrInt::Reg(reg)
            }
        }
    }).collect::<Vec<_>>();
    let mut insts = Vec::new();
    for i in 0..block_params.len() {
        let src = args[i];
        let dst = env.get_reg_or_integer(block_params[i], &mut insts).unwrap();
        let src = match src {
            RegOrInt::Reg(reg) => reg,
            RegOrInt::Int(num) => {
                insts.push(Inst::Li(Reg::from_string(&to_string(dst)), num));
                continue
            }
        };
        if src == dst {
            continue;
        }
        insts.push(Inst::Mv(Reg::from_string(&to_string(dst)), Reg::from_string(&to_string(src))));
    }
    insts
}
impl CodeGen for Value{
    type Output = Vec<Inst>;
    fn code_gen(&self, env: &mut Environment) -> Self::Output {
        let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*self);
        let mut insts = Vec::new();
        match value_data.kind() {
            ValueKind::Binary(binary) => {
                let rs1 = to_string(env.get_reg_with_load(binary.lhs(), T5, &mut insts).unwrap());
                let rs2 = to_string(env.get_reg_with_load(binary.rhs(), T6, &mut insts).unwrap());
                let rd = to_string(env.get_register(*self).unwrap());
                match binary.op() {
                    BinaryOp::Add => insts.push(Inst::Binary(RiscVBinaryOp::Add, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Sub => insts.push(Inst::Binary(RiscVBinaryOp::Sub, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Mul => insts.push(Inst::Binary(RiscVBinaryOp::Mul, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Div => insts.push(Inst::Binary(RiscVBinaryOp::Div, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Mod => insts.push(Inst::Binary(RiscVBinaryOp::Rem, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Gt => insts.push(Inst::Binary(RiscVBinaryOp::Sgt, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Lt => insts.push(Inst::Binary(RiscVBinaryOp::Slt, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2))),
                    BinaryOp::Eq => {
                        insts.push(Inst::Binary(RiscVBinaryOp::Sub, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2)));
                        insts.push(Inst::Seqz(Reg::from_string(&rd), Reg::from_string(&rd)));
                    }
                    BinaryOp::NotEq => {
                        insts.push(Inst::Binary(RiscVBinaryOp::Sub, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2)));
                        insts.push(Inst::Snez(Reg::from_string(&rd), Reg::from_string(&rd)));
                    }
                    BinaryOp::Ge => {
                        insts.push(Inst::Binary(RiscVBinaryOp::Slt, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2)));
                        insts.push(Inst::BinaryImm(RiscVBinaryOp::Xor, Reg::from_string(&rd), Reg::from_string(&rd), 1));
                    }
                    BinaryOp::Le => {
                        insts.push(Inst::Binary(RiscVBinaryOp::Sgt, Reg::from_string(&rd), Reg::from_string(&rs1), Reg::from_string(&rs2)));
                        insts.push(Inst::BinaryImm(RiscVBinaryOp::Xor, Reg::from_string(&rd), Reg::from_string(&rd), 1));
                    }
                    _ => {
                        panic!("Unsupported binary operation: {:?}", binary.op());
                    }
                };
            },
            ValueKind::Jump(jump) => {
                let target = jump.target();
                let target_bb = env.program.func(env.cur_func.unwrap()).dfg().bb(target);
                let target_name = &target_bb.name().as_ref().unwrap()[1..];
                let args = jump.args();
                let block_params = target_bb.params();
                insts.extend(process_jump(args, block_params, env, "t5"));
                insts.push(Inst::Jump(target_name.to_string()));
            }
            ValueKind::Alloc(_) => {
                let size = match value_data.ty().kind() {
                    TypeKind::Pointer(ptr) => {
                        ptr.size()
                    },
                    _ => unreachable!()
                };
                env.cur_pos += size;
                env.insert_stack_variable(*self, env.stack_size - env.cur_pos);
            },
            ValueKind::Store(store) => {
                let val = env.program.func(env.cur_func.unwrap()).dfg().value(store.value());
                if let ValueKind::Aggregate(agg) = val.kind() {
                    let (base, off) = env.get_pos_(store.dest(), "t5", &mut insts).unwrap();
                    for (i, value) in agg.elems().iter().enumerate() {
                        let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*value);
                        let num = match value_data.kind() {
                            ValueKind::Integer(num) => num.value(),
                            _ => unreachable!()
                        };
                        let offset = off + i * 4;
                        let (temp, off) = env.get_offset(base.as_str(), offset as i32, "t6", &mut insts);
                        insts.push(Inst::Li(Reg::from_string("t6"), num));
                        insts.push(Inst::Sw(Reg::from_string("t6"), Reg::from_string(&temp), off as i32));
                    }
                } else {
                    let src = to_string(env.get_reg_with_load(store.value(), T5, &mut insts).unwrap());
                    let (dst, off) = env.get_pos(store.dest(), "t6", &mut insts).unwrap();
                    insts.push(Inst::Sw(Reg::from_string(&src), Reg::from_string(&dst), off as i32));
                }
            }
            ValueKind::Load(load) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let (src, off) = env.get_pos(load.src(), "t5", &mut insts).unwrap();
                insts.push(Inst::Lw(Reg::from_string(&rd), Reg::from_string(&src), off as i32));
            },
            ValueKind::GetElemPtr(ptr) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let (base_reg, off) = env.get_symbol_pos(ptr.src(), "t5", &mut insts).unwrap();
                // assert_eq!(value_data.ty().size(), 4);
                let index = ptr.index();
                let index_data = env.program.func(env.cur_func.unwrap()).dfg().value(index);
                match index_data.kind() {
                    ValueKind::Integer(num) => {
                        let offset = num.value() as usize * 4 + off;
                        // get_addi(rd.as_str(), base_reg.as_str(), offset as i32, "t6")
                        insts.extend(get_addi(rd.as_str(), base_reg.as_str(), offset as i32, "t6"));
                    }
                    _ => {
                        let offset = to_string(env.get_reg_with_load(index, T6, &mut insts).unwrap());
                        insts.push(Inst::Li(Reg::from_string("t6"), 2));
                        insts.push(Inst::Binary(RiscVBinaryOp::Sll, Reg::from_string("t6"), Reg::from_string(&offset), Reg::from_string("t6")));
                        insts.push(Inst::Binary(RiscVBinaryOp::Add, Reg::from_string(&rd), Reg::from_string(&base_reg), Reg::from_string("t6")));
                        insts.extend(get_addi(rd.as_str(), rd.as_str(), off as i32, "t5"));
                    }
                }
            }
            ValueKind::GetPtr(ptr) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let base_reg = to_string(env.get_register(ptr.src()).unwrap());
                // assert_eq!(value_data.ty().size(), 4);
                let index = ptr.index();
                let index_data = env.program.func(env.cur_func.unwrap()).dfg().value(index);
                match index_data.kind() {
                    ValueKind::Integer(num) => {
                        let offset = num.value() as usize * 4;
                        insts.extend(get_addi(rd.as_str(), base_reg.as_str(), offset as i32, "t6"));
                    }
                    _ => {
                        let offset = to_string(env.get_reg_with_load(index, T6, &mut insts).unwrap());
                        insts.push(Inst::Li(Reg::from_string("t6"), 2));
                        insts.push(Inst::Binary(RiscVBinaryOp::Sll, Reg::from_string("t6"), Reg::from_string(&offset), Reg::from_string("t6")));
                        insts.push(Inst::Binary(RiscVBinaryOp::Add, Reg::from_string(&rd), Reg::from_string(&base_reg), Reg::from_string("t6")));
                    }
                }
            },
            ValueKind::Branch(branch) => {
                let cond = to_string(env.get_reg_with_load(branch.cond(), T5, &mut insts).unwrap());
                if cond != "t5" {
                    insts.push(Inst::Mv(Reg::from_string("t5"), Reg::from_string(&cond)));
                }
                let true_bb = branch.true_bb();
                let false_bb = branch.false_bb();
                let true_bb_data = env.program.func(env.cur_func.unwrap()).dfg().bb(true_bb);
                let false_bb_data = env.program.func(env.cur_func.unwrap()).dfg().bb(false_bb);
                let true_name = &true_bb_data.name().as_ref().unwrap()[1..];
                let false_name = &false_bb_data.name().as_ref().unwrap()[1..];
                let true_bb_params = true_bb_data.params();
                let false_bb_params = false_bb_data.params();
                static BRANCH_CNT: AtomicUsize = AtomicUsize::new(0);
                let cnt = BRANCH_CNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                insts.extend(process_jump(branch.false_args(), false_bb_params, env, "t6"));
                insts.push(Inst::Bnez(Reg::from_string("t5"), format!("skip_{}", cnt)));
                insts.push(Inst::Jump(false_name.to_string()));
                insts.push(Inst::Label(format!("skip_{}", cnt)));
                insts.extend(process_jump(branch.true_args(), true_bb_params, env, "t6"));
                insts.push(Inst::Jump(true_name.to_string()));
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let rs = to_string(env.get_reg_with_load(value, T5, &mut insts).unwrap());
                    insts.push(Inst::Mv(Reg::from_string("a0"), Reg::from_string(&rs)));
                }
                // restore sp and ra
                insts.extend(get_addi("sp", "sp", env.stack_size as i32, "t6"));
                let used_regs = get_all_used_reg(&env.register_map);
                for i in 0..12 {
                    let reg = to_string(i);
                    if used_regs[i as usize] {
                        insts.push(Inst::Lw(Reg::from_string(&reg), Reg::from_string("sp"), -((12 + i * 4) as i32)));
                    }
                }
                insts.push(Inst::Lw(Reg::from_string("ra"), Reg::from_string("sp"), -4));
                insts.push(Inst::Lw(Reg::from_string("sp"), Reg::from_string("sp"), -8));
                insts.push(Inst::Ret);
            },
            ValueKind::Call(call) => {
                let active_vals = env.active_map.get(self).unwrap();
                let mut used_regs = get_used_reg(&env.register_map, active_vals);
                let self_reg = env.get_register(*self).unwrap();
                used_regs[self_reg as usize] = false;
                for i in 12..FREE_REG {
                    let reg = to_string(i as Register);
                    if used_regs[i] {
                        env.cur_pos += 4;
                        let offset = env.stack_size - env.cur_pos;
                        let (temp, off) = env.get_offset("sp", offset as i32, "t5", &mut insts);
                        insts.push(Inst::Sw(Reg::from_string(&reg), Reg::from_string(&temp), off as i32));
                    }
                }

                let callee_data = env.program.func(call.callee());
                let func_name = &callee_data.name()[1..];
                for (i, arg) in call.args().iter().enumerate().rev() {
                    if i < 8 {
                        let temp = A0 + i as Register;
                        let reg = to_string(env.get_reg_with_load(*arg, temp, &mut insts).unwrap());
                        if reg != to_string(temp) {
                            insts.push(Inst::Mv(Reg::from_string(&to_string(temp)), Reg::from_string(&reg)));
                        }
                    } else {
                        let reg = to_string(env.get_reg_with_load(*arg, T5, &mut insts).unwrap());
                        let offset = 4 * (i - 8);
                        let (temp, off) = env.get_offset("sp", offset as i32, "t6", &mut insts);
                        insts.push(Inst::Sw(Reg::from_string(&reg), Reg::from_string(&temp), off as i32));
                    }
                }
                insts.push(Inst::Call(func_name.to_string()));

                let return_type = callee_data.ty();
                match return_type.kind() {
                    TypeKind::Function(_, ret) => {
                        if ret.is_i32() {
                            let rd = to_string(env.get_register(*self).unwrap());
                            insts.push(Inst::Mv(Reg::from_string(&rd), Reg::from_string("a0")));
                        }
                    }
                    _ => {unreachable!()}
                };

                for i in (12..FREE_REG).rev() {
                    let reg = to_string(i as Register);
                    if used_regs[i] {
                        let offset = env.stack_size - env.cur_pos;
                        env.cur_pos -= 4;
                        let (temp, off) = env.get_offset("sp", offset as i32, "t5", &mut insts);
                        insts.push(Inst::Lw(Reg::from_string(&reg), Reg::from_string(&temp), off as i32));
                    }
                }
            }
            _ => {
                panic!("Unsupported value kind");
            }
        };
        insts
    }
}