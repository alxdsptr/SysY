use koopa::ir::{Program, Type, TypeKind, ValueKind};
use koopa::opt::ModulePass;
use std::collections::HashSet;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use crate::backend::register::get_referenced_value;

pub struct DeadCodeElimination;
impl DeadCodeElimination {
    pub fn new() -> Self {
        DeadCodeElimination
    }
}
fn add_terminator(program: &mut Program) {
    for func in program.funcs_mut().values_mut() {
        let mut terminators = HashSet::new();

        for (value, value_data) in func.dfg().values() {
            if matches!(value_data.kind(), ValueKind::Return(_) | ValueKind::Jump(_) | ValueKind::Branch(_)) {
                terminators.insert(*value);
            }
        }
        let mut bb_cursor = func.layout_mut().bbs_mut().cursor_front_mut();
        let mut bb_list = Vec::new();
        while let Some(bb) = bb_cursor.node_mut() {
            let mut inst_cursor = bb.insts_mut().cursor_front_mut();
            while let Some(inst) = inst_cursor.key() {
                if terminators.contains(inst) {
                    inst_cursor.move_next();
                    while let Some((_, _)) = inst_cursor.remove_current() {}
                    break;
                }

                inst_cursor.move_next();
            }

            if !bb.insts().back_key().is_some_and(|val| {terminators.contains(val)}) {
                bb_list.push(*bb_cursor.key().unwrap());
            }

            bb_cursor.move_next();
        }
        if let TypeKind::Function(_, ty) = func.ty().kind() {
            if *ty == Type::get_unit() {
                for bb in bb_list {
                    let ret_inst = func.dfg_mut().new_value().ret(None);
                    let bb_node = func.layout_mut().bbs_mut().node_mut(&bb).unwrap();
                    bb_node.insts_mut().push_key_back(ret_inst).unwrap();
                }
            }
            else {
                for bb in bb_list {
                    let zero = func.dfg_mut().new_value().integer(0);
                    let ret_inst = func.dfg_mut().new_value().ret(Some(zero));
                    let bb_node = func.layout_mut().bbs_mut().node_mut(&bb).unwrap();
                    bb_node.insts_mut().push_key_back(ret_inst).unwrap();
                }
            }
        }
    }
}
fn delete_unreachable_bb(program: &mut Program) {
    for func in program.funcs_mut().values_mut() {
        if func.layout().entry_bb().is_none() {
            continue;
        }
        let mut arrivable = HashSet::new();
        let entry_bb = func.layout_mut().entry_bb().unwrap();
        arrivable.insert(entry_bb);
        let mut stack = vec![entry_bb];
        while let Some(bb) = stack.pop() {
            let bb_node = func.layout().bbs().node(&bb).unwrap();
            let end = bb_node.insts().back_key().unwrap();
            let end = func.dfg().value(*end);
            match end.kind() {
                ValueKind::Branch(branch) => {
                    let tr = branch.true_bb();
                    let fa = branch.false_bb();
                    if arrivable.insert(tr) {
                        stack.push(tr);
                    }
                    if arrivable.insert(fa) {
                        stack.push(fa);
                    }
                }
                ValueKind::Jump(jump) => {
                    let target = jump.target();
                    if arrivable.insert(target) {
                        stack.push(target);
                    }
                }
                _ => {}
            };
        }

        let mut bb_cursor = func.layout_mut().bbs_mut().cursor_front_mut();
        while let Some(bb) = bb_cursor.key() {
            if !arrivable.contains(bb) {
                bb_cursor.remove_current();
            } else {
                bb_cursor.move_next();
            }
        }
    }
}

fn delete_useless_vals(program: &mut Program) {
    let globals = program.inst_layout();
    let mut global_map = HashSet::new();
    for val in globals {
        global_map.insert(*val);
    }

    let funcs = Vec::from(program.func_layout());
    for func in funcs {
        let func_data = program.func(func);
        let mut useful_vals = HashSet::new();
        let mut stack = Vec::new();
        for (_, bb) in  func_data.layout().bbs() {
            for val in bb.insts().keys() {
                let val_data = func_data.dfg().value(*val);
                let need = match val_data.kind() {
                    ValueKind::Return(_) | ValueKind::Call(_) |  ValueKind::Branch(_) |ValueKind::Jump(_) | ValueKind::Alloc(_) | ValueKind::Store(_) => {
                        true
                    }
                    _ => {false}
                };
                if need {
                    if useful_vals.insert(*val) {
                        stack.push(*val);
                    }
                }
            }
        }
        while let Some(val) = stack.pop() {
            if global_map.contains(&val) {
                continue;
            }
            let val_data = func_data.dfg().value(val);
            let temp = get_referenced_value(val_data);
            for v in temp {
                if useful_vals.insert(v) {
                    stack.push(v);
                }
            }
        }
        let func_data = program.func_mut(func);
        let mut bb_cursor = func_data.layout_mut().bbs_mut().cursor_front_mut();
        while let Some(bb) = bb_cursor.node_mut() {
            let mut inst_cursor = bb.insts_mut().cursor_front_mut();
            while let Some(inst) = inst_cursor.key() {
                if !useful_vals.contains(inst) {
                    inst_cursor.remove_current();
                } else {
                    inst_cursor.move_next();
                }
            }
            bb_cursor.move_next();
        }
    }
}
impl ModulePass for DeadCodeElimination {
    fn run_on(&mut self, program: &mut Program) {
        add_terminator(program);
        delete_unreachable_bb(program);
        delete_useless_vals(program);
    }
}