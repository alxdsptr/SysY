use std::cmp::min;
use std::fmt::format;
use std::fs::{create_dir_all, metadata, File};
use std::io::Write;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value};
use koopa::ir::entities::ValueData;
use koopa::ir::ValueKind;
use koopa::ir::BinaryOp;
use crate::backend::environment::Environment;
use crate::backend::register::{to_string, A6, A7};

pub trait CodeGen {
    fn code_gen(&self, env: &mut Environment);
}

impl CodeGen for Program {
    fn code_gen(&self, env: &mut Environment) {
        // data section
        env.output.write_all("  .data\n".as_bytes()).unwrap();
        for &val in self.inst_layout() {
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
                            let size = value_data.ty().size();
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
            env.enter_new_func(*func);
            func_data.code_gen(env);
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
                ValueKind::Alloc(alloc) => {
                    let size = value_data.ty().size();
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
    let param_count = func.params().len();
    if param_count > 8 {
        stack_size -= (param_count - 8) * 4; // these params are stored in the stack of the caller
    }
    if max_arg_num > 8 {
        stack_size += (max_arg_num - 8) * 4; // these args are stored in the stack of the callee
    }
    stack_size += (env.max_reg_num * 4) as usize; // reserve space for registers
    stack_size += 8; // return address and sp
    // align stack to 16 bytes
    stack_size = (stack_size + 15) & (!0xF);
    stack_size
}
impl CodeGen for FunctionData {
    fn code_gen(&self, env: &mut Environment) {
        if self.layout().entry_bb().is_none() {
            return;
        }
        env.output.write_all(format!("  .globl {}\n", &self.name()[1..]).as_bytes()).unwrap();
        env.output.write_all(format!("{}:\n", &self.name()[1..]).as_bytes()).unwrap();

        // store ra and sp to stack
        env.output.write_all("  sw ra, -4(sp)\n".as_bytes()).unwrap();
        env.output.write_all("  sw sp, -8(sp)\n".as_bytes()).unwrap();

        for i in 0..min(12, env.max_reg_num) {
            let reg = to_string(i);
            env.output.write_all(format!("  sw {}, -{}(sp)\n", reg, 12 + i * 4).as_bytes()).unwrap();
        }

        let stack_size = compute_stack_size(self, env);
        env.output.write_all(format!("  addi sp, sp, -{}\n", stack_size).as_bytes()).unwrap();
        env.stack_size = stack_size;
        env.cur_pos = (8 + min(12, env.max_reg_num) * 4) as usize; // 8 for ra and sp, 4 for each register
        let entry_bb = self.layout().entry_bb().unwrap();
        {
            let bb_data = env.program.func(env.cur_func.unwrap()).dfg().bb(entry_bb);
            env.output.write_all(format!("{}:\n", &bb_data.name().as_ref().unwrap()[1..]).as_bytes()).unwrap();
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
            env.insert_stack_variable(*inst, offset);

            cursor.move_next();
            let inst = cursor.key().unwrap();
            let value_data = env.program.func(env.cur_func.unwrap()).dfg().value(*inst);
            assert!(matches!(value_data.kind(), ValueKind::Store(_)));
            if i < 8 {
                // argument in ai register, store in pos offset(sp)
                env.output.write_all(format!("  sw a{}, {}(sp)\n", i, offset).as_bytes()).unwrap();
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
            env.output.write_all(format!("{}:\n", &bb_data.name().as_ref().unwrap()[1..]).as_bytes()).unwrap();
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
                let rs1 = to_string(env.get_reg_with_load(binary.lhs(), A6).unwrap());
                let rs2 = to_string(env.get_reg_with_load(binary.rhs(), A7).unwrap());
                let rd = to_string(env.get_register(*self).unwrap());

                match binary.op() {
                    BinaryOp::Add => format!("  add {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Sub => format!("  sub {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mul => format!("  mul {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Div => format!("  div {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mod => format!("  rem {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Gt => format!("  sgt {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Lt => format!("  slt {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Eq => format!("  sub {}, {}, {}\n  seqz {}, {}\n", rd, rs1, rs2, rd, rd),
                    BinaryOp::NotEq => format!("  sub {}, {}, {}\n  snez {}, {}\n", rd, rs1, rs2, rd, rd),
                    BinaryOp::Ge => format!("  slt {}, {}, {}\n  xori {}, {}, {}\n", rd, rs1, rs2, rd, rd, 1),
                    BinaryOp::Le => format!("  sgt {}, {}, {}\n  xori {}, {}, {}\n", rd, rs1, rs2, rd, rd, 1),
                    _ => {
                        panic!("Unsupported binary operation: {:?}", binary.op());
                    }
                }
            },
            ValueKind::Jump(jump) => {
                let target = jump.target();
                let target_name = &env.program.func(env.cur_func.unwrap()).dfg().bb(target).name().as_ref().unwrap()[1..];
                format!("  j {}\n", target_name)
            }
            ValueKind::Alloc(_) => {
                let size = value_data.ty().size();
                env.cur_pos += size;
                env.insert_stack_variable(*self, env.stack_size - env.cur_pos);
                String::new()
            },
            ValueKind::Store(store) => {
                let src = to_string(env.get_reg_with_load(store.value(), A6).unwrap());
                let dst = env.get_pos(store.dest(), "a6").unwrap();
                format!("  sw {}, {}\n", src, dst)
            }
            ValueKind::Load(load) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let src = env.get_pos(load.src(), "a6").unwrap();
                format!("  lw {}, {}\n", rd, src)
            },
            ValueKind::GetElemPtr(ptr) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let (base_reg, off) = env.get_symbol_pos(ptr.src(), "a6").unwrap();
                // assert_eq!(value_data.ty().size(), 4);
                let index = ptr.index();
                let index_data = env.program.func(env.cur_func.unwrap()).dfg().value(index);
                match index_data.kind() {
                    ValueKind::Integer(num) => {
                        let offset = num.value() as usize * 4 + off;
                        format!("  lw {}, {}({})\n", rd, offset, base_reg)
                    }
                    _ => {
                        let offset = to_string(env.get_reg_with_load(index, A7).unwrap());
                        format!("  li a7, 2\n  sll a7, {}, a7\n\
                          addi {}, {}, a7\n", offset, rd, base_reg)
                    }
                }
            }
            ValueKind::GetPtr(ptr) => {
                let rd = to_string(env.get_register(*self).unwrap());
                let base_reg = env.get_register(ptr.src()).unwrap();
                // assert_eq!(value_data.ty().size(), 4);
                let index = ptr.index();
                let index_data = env.program.func(env.cur_func.unwrap()).dfg().value(index);
                match index_data.kind() {
                    ValueKind::Integer(num) => {
                        let offset = num.value() as usize * 4;
                        format!("  lw {}, {}({})\n", rd, offset, base_reg)
                    }
                    _ => {
                        let offset = to_string(env.get_reg_with_load(index, A7).unwrap());
                        format!("  li a7, 2\n  sll a7, {}, a7\n\
                          addi {}, {}, a7\n", offset, rd, base_reg)
                    }
                }
            },
            ValueKind::Branch(branch) => {
                let cond = to_string(env.get_reg_with_load(branch.cond(), A6).unwrap());
                let true_bb = branch.true_bb();
                let false_bb = branch.false_bb();
                let true_name = &env.program.func(env.cur_func.unwrap()).dfg().bb(true_bb).name().as_ref().unwrap()[1..];
                let false_name = &env.program.func(env.cur_func.unwrap()).dfg().bb(false_bb).name().as_ref().unwrap()[1..];
                format!("  bnez {}, {}\n  j {}\n", cond, true_name, false_name)
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let rs = to_string(env.get_reg_with_load(value, A6).unwrap());
                    env.output.write_all(format!("  mv a0, {}\n", rs).as_bytes()).unwrap();
                }
                // restore sp and ra
                env.output.write_all(format!("  addi sp, sp, {}\n", env.stack_size).as_bytes()).unwrap();
                env.output.write_all("  lw ra, -4(sp)\n".as_bytes()).unwrap();
                env.output.write_all("  lw sp, -8(sp)\n".as_bytes()).unwrap();

                for i in 0..min(12, env.max_reg_num) {
                    let reg = to_string(i);
                    env.output.write_all(format!("  lw {}, -{}(sp)\n", reg, 12 + i * 4).as_bytes()).unwrap();
                }
                env.output.write_all("  ret\n".as_bytes()).unwrap();
                return;
            },
            ValueKind::Call(call) => {
                if env.max_reg_num > 12 {
                    for i in 12..env.max_reg_num {
                        let reg = to_string(i);
                        // let offset = env.stack_size - env.cur_pos - 4 * (i - 12);
                        env.cur_pos += 4;
                        let offset = env.stack_size - env.cur_pos;
                        env.output.write_all(format!("  sw {}, {}(sp)\n", reg, offset).as_bytes()).unwrap();
                    }
                }

                let callee_data = env.program.func(call.callee());
                let func_name = &callee_data.name()[1..];
                let mut args = Vec::new();
                for arg in call.args() {
                    let reg = env.get_reg_with_load(*arg, A6).unwrap();
                    args.push(to_string(reg));
                }
                for (i, arg) in args.iter().enumerate().skip(8) {
                    if i > 8 {
                        let offset = 4 * (i - 8);
                        env.output.write_all(format!("  sw {}, {}(sp)\n", arg, offset).as_bytes()).unwrap();
                    } else {
                        // move args to a0-a7
                        env.output.write_all(format!("  mv a{}, {}\n", i, arg).as_bytes()).unwrap();
                    }
                }
                env.output.write_all(format!("  call {}\n", func_name).as_bytes()).unwrap();

                let return_type = callee_data.ty();
                if return_type.is_i32() {
                    let rd = to_string(env.get_register(*self).unwrap());
                    env.output.write_all(format!("  mv {}, a0\n", rd).as_bytes()).unwrap();
                }

                if env.max_reg_num > 12 {
                    for i in 12..env.max_reg_num {
                        let reg = to_string(i);
                        let offset = env.stack_size - env.cur_pos;
                        env.cur_pos -= 4;
                        env.output.write_all(format!("  lw {}, {}(sp)\n", reg, offset).as_bytes()).unwrap();
                    }
                }
                String::new()
            }
            _ => {
                panic!("Unsupported value kind");
            }
        };
        env.output.write_all(str.as_bytes()).unwrap();
    }
}