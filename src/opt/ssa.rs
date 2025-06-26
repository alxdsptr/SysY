use std::collections::{HashMap, HashSet};
use koopa::ir::{BasicBlock, FunctionData, Type, TypeKind, Value};
use koopa::ir::builder::BasicBlockBuilder;
use koopa::opt::ModulePass;
use koopa::ir::ValueKind;
use koopa::ir::values::BlockArgRef;
use crate::backend::register::{get_pred_and_end, get_topo_order};

pub struct ToSSA;
struct Environment {
    old_vars: HashMap<Value, Option<Value>>,
    cur_vars: HashMap<Value, Value>,
}
impl Environment {
    pub fn new() -> Environment {
        Environment {
            old_vars: HashMap::new(),
            cur_vars: HashMap::new(),
        }
    }
    pub fn get(&self, val: &Value) -> Option<Value> {
        match self.cur_vars.get(val) {
            Some(v) => Some(*v),
            None => self.old_vars.get(val).cloned().unwrap_or(None),
        }
    }
    pub fn set(&mut self, key: Value, val: Value) {
        if self.cur_vars.contains_key(&key) {
            self.cur_vars.insert(key, val);
        } else {
            if self.old_vars.contains_key(&key) {
                self.old_vars.insert(key, Some(val));
            } else {
                unreachable!("key not found");
            }
        }
    }
    pub fn insert_old(&mut self, key: Value, val: Option<Value>) {
        self.old_vars.insert(key, val);
    }
    pub fn insert_new(&mut self, key: Value, val: Value) {
        self.cur_vars.insert(key, val);
    }
}

fn process_bb(
    bb: BasicBlock,
    func_data: &mut FunctionData,
    pred: &HashMap<BasicBlock, Vec<BasicBlock>>,
) {
    // let temp = func_data.layout_mut().bb_mut(bb);
    let temp = func_data.dfg_mut().bb_mut(bb);
    let pred = temp.used_by();
}
impl ModulePass for ToSSA {
    fn run_on(&mut self, program: &mut koopa::ir::Program) {
        let functions = program.funcs().keys().cloned().collect::<Vec<_>>();
        for func in functions {
            let func_data = program.func_mut(func);
            let bbs = func_data.layout().bbs().keys().cloned().collect::<Vec<_>>();
            let mut val_to_name: HashMap<Value, String> = HashMap::new();
            let mut name_to_val:HashMap<String, (Value, Type)> = HashMap::new();
            let mut allocated_variables: HashMap<BasicBlock, HashSet<String>> = HashMap::new();
            let mut self_allocated: HashMap<BasicBlock, HashSet<String>> = HashMap::new();
            let mut alloc_inst = HashSet::new();
            for bb in &bbs {
                let bb_node = func_data.layout().bbs().node(bb).unwrap();
                for val in bb_node.insts().keys() {
                    let value_data = func_data.dfg().value(*val);
                    match value_data.kind() {
                        ValueKind::Alloc(_) => {
                            match value_data.ty().kind() {
                                TypeKind::Pointer(ptr) => {
                                    if TypeKind::Int32 == *ptr.kind() {
                                        let name = value_data.name().as_ref().unwrap().to_string();
                                        val_to_name.insert(*val, name.clone());
                                        name_to_val.insert(name.clone(), (*val, ptr.clone()));
                                        allocated_variables.entry(*bb).or_default().insert(name.clone());
                                        self_allocated.entry(*bb).or_default().insert(name);
                                        alloc_inst.insert(val);
                                    }
                                },
                                _ => unreachable!(),
                            }
                        },
                        _ => {}
                    }
                }
                let end_val = bb_node.insts().back_key().unwrap();
                let end_data = func_data.dfg().value(*end_val);
                let cur_vars = allocated_variables.entry(*bb).or_default().clone();
                match end_data.kind() {
                    ValueKind::Jump(jump) => {
                        let bb = jump.target();
                        allocated_variables.entry(bb).or_default().extend(cur_vars);
                    },
                    ValueKind::Branch(branch) => {
                        let true_bb = branch.true_bb();
                        let false_bb = branch.false_bb();
                        allocated_variables.entry(true_bb).or_default().extend(cur_vars.iter());
                        allocated_variables.entry(false_bb).or_default().extend(cur_vars);
                    },
                    _ => {}
                };
            }

            let (pred, end_bb) = get_pred_and_end(program, func);
            let order = get_topo_order(&end_bb, &pred);
            let mut changed = true;
            while changed {
                changed = false;
                for bb in order.iter() {
                    let pred_bbs = pred.get(bb).unwrap();
                    let old_size = match allocated_variables.get(bb) {
                        Some(vars) => vars.len(),
                        None => 0
                    };
                    for pred in pred_bbs.iter() {
                        let pre_vars = allocated_variables.entry(*pred).or_default();
                        allocated_variables.entry(*bb).or_default().extend(pre_vars);
                    }
                    let new_size = match allocated_variables.get(bb) {
                        Some(vars) => vars.len(),
                        None => 0
                    };
                    if new_size > old_size {
                        changed = true;
                    }
                }
            }

            for bb in &bbs {
                match allocated_variables.get_mut(bb) {
                    Some(all_vars) => {
                        let self_vars = self_allocated.get(bb).unwrap_or(&HashSet::new());
                        for var in self_vars {
                            all_vars.remove(var);
                        }
                    }
                    None => continue
                }
            }
unsafe {

            let mut env = Environment::new();
            let mut delete_bb = HashSet::new();
            for bb in &bbs {
                let bb_data = func_data.dfg_mut().bb_mut(*bb);
                let name = bb_data.name();
                let params = match allocated_variables.get(bb) {
                    Some(vars) => vars,
                    None => continue,
                };
                delete_bb.insert(bb);
                let self_vars = self_allocated.get(bb).unwrap_or(&HashSet::new());
                let mut index_map: HashMap<usize, Value> = HashMap::new();
                for var in self_vars {
                    let val = name_to_val.get(var).unwrap().0;
                    env.insert_old(val, None);
                }
                let params = params.iter().enumerate().map(|(i, str)| {
                    let (val, ty) = name_to_val.get(str).unwrap();
                    index_map.insert(i, *val);
                    ty.clone()
                }).collect::<Vec<_>>();
                let new_bb = func_data.dfg_mut().new_bb().basic_block_with_params(name.clone(), params);
                func_data.layout_mut().bbs_mut().push_key_back(new_bb).unwrap();
                let new_bb = func_data.dfg_mut().bb_mut(new_bb);
                for (i, val) in new_bb.params().iter().enumerate() {
                    env.insert_new(*index_map.get(&i).unwrap(), *val);
                }


                let bb_node = func_data.layout().bbs().node(bb).unwrap();
                let mut new_insts = Vec::new();
                for val in bb_node.insts().keys() {
                    let value_data = func_data.dfg().value(*val);
                    if alloc_inst.contains(val) {
                        continue;
                    }
                    let value_data = func_data.dfg_mut().value(*val);
                    match value_data.kind() {
                        ValueKind::Store(store) => {
                            let val = store.value();
                            let dest = store.dest();
                            if val_to_name.contains_key(&dest) {
                                env.set(dest, val);
                            } else {
                                new_insts.push(val);
                            }
                        }, ValueKind::Load(load) => {
                            // replace
                        }
                    }
                }
            }
        }

}

    }
}