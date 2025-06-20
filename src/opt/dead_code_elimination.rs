use koopa::ir::{Program, Type, TypeKind, ValueKind};
use koopa::opt::ModulePass;
use std::collections::HashSet;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};

pub struct DeadCodeElimination;

impl ModulePass for DeadCodeElimination {
    fn run_on(&mut self, program: &mut Program) {
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
            if func.name() == "@main" {
                for bb in bb_list {
                    let zero = func.dfg_mut().new_value().integer(0);
                    let ret_inst = func.dfg_mut().new_value().ret(Some(zero));
                    let bb_node = func.layout_mut().bbs_mut().node_mut(&bb).unwrap();
                    bb_node.insts_mut().push_key_back(ret_inst).unwrap();
                }
            } else if let TypeKind::Function(_, ty) = func.ty().kind() {
                if *ty == Type::get_unit() {
                    for bb in bb_list {
                        let ret_inst = func.dfg_mut().new_value().ret(None);
                        let bb_node = func.layout_mut().bbs_mut().node_mut(&bb).unwrap();
                        bb_node.insts_mut().push_key_back(ret_inst).unwrap();
                    }

                } /*else {
                    let mut delete_bb = HashSet::new();
                    for bb in bb_list {
                        delete_bb.insert(bb);
                    }
                    let mut cursor = func.layout_mut().bbs_mut().cursor_front_mut();
                    while let Some(bb) = cursor.key() {
                        if delete_bb.contains(bb) {
                            cursor.remove_current().unwrap();
                        } else {
                            cursor.move_next();
                        }
                    }
                }*/
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
}