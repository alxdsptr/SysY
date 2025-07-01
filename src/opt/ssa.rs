use std::collections::{HashMap, HashSet};
use koopa::ir;
use koopa::ir::{BasicBlock, FunctionData, Type, TypeKind, Value};
use koopa::ir::builder::{BasicBlockBuilder, EntityInfoQuerier, LocalInstBuilder, ValueBuilder};
use koopa::ir::entities::ValueData;
use koopa::opt::ModulePass;
use koopa::ir::ValueKind;
use koopa::ir::values::BlockArgRef;
use crate::backend::register::{get_active_values, get_pred_and_end, get_topo_order};
use crate::opt::dead_code_elimination::DeadCodeElimination;

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
struct NameGenerator {
    names: HashMap<String, usize>,
}
impl NameGenerator {
    pub fn new() -> NameGenerator {
        NameGenerator {
            names: HashMap::new(),
        }
    }
    pub fn get_name(&mut self, name: &str) -> String {
        let count = self.names.entry(name.to_string()).or_insert(0);
        *count += 1;
        if *count == 1 {
            name.to_string()
        } else {
            format!("{}_{}", name, count)
        }
    }
}
fn replace_inst(func_data: &mut FunctionData, val: Value, val_map: &mut HashMap<Value, Value>,
    get_new_bb_target: &dyn Fn(BasicBlock, &[Value], &Environment, &HashMap<Value, Value>, &mut bool) -> (BasicBlock, Vec<Value>)) {
    let value_kind = func_data.dfg().value(val).kind().clone();
    let get_new_val = |val: Value, changed: &mut bool| {
        match val_map.get(&val) {
            Some(v) => {
                *changed = true;
                *v
            }
            None => val
        }
    };
    match value_kind {
        ValueKind::Store(store) => {
            let mut changed = false;
            let new_value = get_new_val(store.value(), &mut changed);
            let new_dest = get_new_val(store.dest(), &mut changed);
            if changed {
                func_data.dfg_mut().replace_value_with(val).store(new_value, new_dest);
            }
        },
        ValueKind::Load(load) => {
            let mut changed = false;
            let new_src = get_new_val(load.src(), &mut changed);
            if changed {
                func_data.dfg_mut().replace_value_with(val).load(new_src);
            }
        },
        ValueKind::Binary(binary) => {
            let mut changed = false;
            let new_lhs = get_new_val(binary.lhs(), &mut changed);
            let new_rhs = get_new_val(binary.rhs(), &mut changed);
            if changed {
                func_data.dfg_mut().replace_value_with(val).binary(binary.op(), new_lhs, new_rhs);
            }
        },
        ValueKind::GetElemPtr(ptr) => {
            let mut changed = false;
            let new_src = get_new_val(ptr.src(), &mut changed);
            let new_index = get_new_val(ptr.index(), &mut changed);
            if changed {
                func_data.dfg_mut().replace_value_with(val).get_elem_ptr(new_src, new_index);
            }
        },
        ValueKind::GetPtr(ptr) => {
            let mut changed = false;
            let new_src = get_new_val(ptr.src(), &mut changed);
            let new_index = get_new_val(ptr.index(), &mut changed);
            if changed {
                func_data.dfg_mut().replace_value_with(val).get_ptr(new_src, new_index);
            }
        },
        ValueKind::Call(call) => {
            let mut changed = false;
            let new_args = call.args().iter().map(|arg| {
                get_new_val(*arg, &mut changed)
            }).collect::<Vec<_>>();
            if changed {
                func_data.dfg_mut().replace_value_with(val).call(call.callee(), new_args);
            }
        },
        ValueKind::Return(ret) => {
            let mut changed = false;
            if let Some(ret_val) = ret.value() {
                let new_val = get_new_val(ret_val, &mut changed);
                if changed {
                    func_data.dfg_mut().replace_value_with(val).ret(Some(new_val));
                }
            }
        },
        ValueKind::Branch(branch) => {
            let mut changed = false;
            let new_cond = get_new_val(branch.cond(), &mut changed);
            let (new_true_bb, true_args) = get_new_bb_target(branch.true_bb(), branch.true_args(), &Environment::new(), val_map, &mut changed);
            let (new_false_bb, false_args) = get_new_bb_target(branch.false_bb(), branch.false_args(), &Environment::new(), val_map, &mut changed);
            println!("new branch, true_bb: {:?}, false_bb: {:?}", new_true_bb, new_false_bb);
            if changed {
                func_data.dfg_mut().replace_value_with(val).branch_with_args(new_cond,
                                                                              new_true_bb, new_false_bb, true_args, false_args);
            }
        },
        ValueKind::Jump(jump) => {
            let mut changed = false;
            let (new_target, args) = get_new_bb_target(jump.target(), jump.args(), &Environment::new(), val_map, &mut changed);
            println!("new jump target: {:?}", new_target);
            if changed {
                func_data.dfg_mut().replace_value_with(val).jump_with_args(new_target, args);
            }
        },
        _ => {}
    }

}

fn compute_binary(op: ir::BinaryOp, lhs: i32, rhs: i32) -> i32 {
    match op {
        ir::BinaryOp::Add => lhs + rhs,
        ir::BinaryOp::Sub => lhs - rhs,
        ir::BinaryOp::Mul => lhs * rhs,
        ir::BinaryOp::Div => lhs / rhs,
        ir::BinaryOp::Mod => lhs % rhs,
        ir::BinaryOp::Eq => if lhs == rhs { 1 } else { 0 },
        ir::BinaryOp::NotEq => if lhs != rhs { 1 } else { 0 },
        ir::BinaryOp::Gt => if lhs > rhs { 1 } else { 0 },
        ir::BinaryOp::Ge => if lhs >= rhs { 1 } else { 0 },
        ir::BinaryOp::Lt => if lhs < rhs { 1 } else { 0 },
        ir::BinaryOp::Le => if lhs <= rhs { 1 } else { 0 },
        _ => unreachable!("Unsupported binary operation: {:?}", op),
    }
}

fn process_inst(func_data: &mut FunctionData, val: Value, val_map: &mut HashMap<Value, Value>,
                new_insts: &mut Vec<Value>, get_new_bb_target: &dyn Fn(BasicBlock, &[Value], &Environment, &HashMap<Value, Value>, &mut bool) -> (BasicBlock, Vec<Value>),
                env: &Environment, first: bool
) {
    let value_data = func_data.dfg().value(val).clone();
    let get_new_val = |val: Value, changed: &mut bool| {
        match val_map.get(&val) {
            Some(v) => {
                *changed = true;
                *v
            }
            None => val
        }
    };
    match value_data.kind() {
        ValueKind::Store(store) => {
            let mut changed = false;
            let new_value = get_new_val(store.value(), &mut changed);
            let new_dest = get_new_val(store.dest(), &mut changed);
            if changed {
                let new_store = func_data.dfg_mut().new_value().store(new_value, new_dest);
                new_insts.push(new_store);
            } else {
                new_insts.push(val);
            }
        }
        ValueKind::Load(load) => {
            let mut changed = false;
            let new_src = get_new_val(load.src(), &mut changed);
            if changed {
                let new_load = func_data.dfg_mut().new_value().load(new_src);
                val_map.insert(val, new_load);
                new_insts.push(new_load);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::Binary(binary) => {
            let mut changed = false;
            let new_lhs = get_new_val(binary.lhs(), &mut changed);
            let new_rhs = get_new_val(binary.rhs(), &mut changed);
            let new_lhs_data = func_data.dfg().value(new_lhs);
            let lhs: Option<i32> = match new_lhs_data.kind() {
                ValueKind::Integer(lhs) => Some(lhs.value()),
                _ => None,
            };
            let new_rhs_data = func_data.dfg().value(new_rhs);
            let rhs: Option<i32> = match new_rhs_data.kind() {
                ValueKind::Integer(rhs) => Some(rhs.value()),
                _ => None,
            };
            if lhs.is_some() && rhs.is_some() {
                changed = true;
            }
            if changed {
                let new_val = if lhs.is_some() && rhs.is_some() {
                    let lhs = lhs.unwrap();
                    let rhs = rhs.unwrap();
                    let new_val = compute_binary(binary.op(), lhs, rhs);
                    func_data.dfg_mut().new_value().integer(new_val)
                } else {
                    let new_val = func_data.dfg_mut().new_value().binary(binary.op(), new_lhs, new_rhs);
                    new_insts.push(new_val);
                    new_val
                };
                val_map.insert(val, new_val);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::GetElemPtr(ptr) => {
            let mut changed = false;
            let new_src = get_new_val(ptr.src(), &mut changed);
            let new_index = get_new_val(ptr.index(), &mut changed);
            if changed {
                let new_val = func_data.dfg_mut().new_value().get_elem_ptr(new_src, new_index);
                val_map.insert(val, new_val);
                new_insts.push(new_val);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::GetPtr(ptr) => {
            let mut changed = false;
            let new_src = get_new_val(ptr.src(), &mut changed);
            let new_index = get_new_val(ptr.index(), &mut changed);
            if changed {
                let new_val = func_data.dfg_mut().new_value().get_ptr(new_src, new_index);
                val_map.insert(val, new_val);
                new_insts.push(new_val);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::Call(call) => {
            let mut changed = false;
            let new_args = call.args().iter().map(|arg| {
                let new_val =get_new_val(*arg, &mut changed);
                println!("old_val: {:?}, new_val: {:?}", arg, new_val);
                new_val
            }).collect::<Vec<_>>();
            if changed {
                let new_val = func_data.dfg_mut().new_value().call(call.callee(), new_args);
                val_map.insert(val, new_val);
                new_insts.push(new_val);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::Return(ret) => {
            let mut changed = false;
            if let Some(ret_val) = ret.value() {
                let new_val = get_new_val(ret_val, &mut changed);
                if changed {
                    let new_ret = func_data.dfg_mut().new_value().ret(Some(new_val));
                    new_insts.push(new_ret);
                } else {
                    new_insts.push(val);
                }
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::Branch(branch) => {
            let mut changed = false;
            let new_cond = get_new_val(branch.cond(), &mut false);
            let (new_true_bb, true_args) = get_new_bb_target(branch.true_bb(), branch.true_args(), env, val_map, &mut changed);
            let (new_false_bb, false_args) = get_new_bb_target(branch.false_bb(), branch.false_args(), env, val_map, &mut changed);
            if let ValueKind::Integer(num) = func_data.dfg().value(new_cond).kind() {
                changed = true;
            }
            if changed {
                let new_inst = if let ValueKind::Integer(num) = func_data.dfg().value(new_cond).kind() {
                    if num.value() == 0 {
                        func_data.dfg_mut().new_value().jump_with_args(new_false_bb, false_args)
                    } else {
                        func_data.dfg_mut().new_value().jump_with_args(new_true_bb, true_args)
                    }
                } else {
                    println!("new branch, true_bb: {:?}, false_bb: {:?}", new_true_bb, new_false_bb);
                    func_data.dfg_mut().new_value().branch_with_args(new_cond, new_true_bb, new_false_bb, true_args, false_args)
                };
                new_insts.push(new_inst);
            } else {
                new_insts.push(val);
            }
        },
        ValueKind::Jump(jump) => {
            let mut changed = false;
            let (new_target, args) = get_new_bb_target(jump.target(), jump.args(), env, val_map, &mut changed);
            if changed {
                let new_jump = func_data.dfg_mut().new_value().jump_with_args(new_target, args);
                println!("new jump target: {:?}", new_target);
                new_insts.push(new_jump);
            } else {
                new_insts.push(val);
            }
        },
        _ => {
            new_insts.push(val);
        }
    }
}
fn delete_bbs(func_data: &mut FunctionData, delete_bb: &HashSet<BasicBlock>) {
    let mut bb_cursor = func_data.layout_mut().bbs_mut().cursor_front_mut();
    while let Some(bb) = bb_cursor.key() {
        if delete_bb.contains(bb) {
            bb_cursor.remove_current();
        } else {
            bb_cursor.move_next();
        }
    }
}
fn is_alloc(val: &ValueData) -> Result<bool, String> {
    match val.kind() {
        ValueKind::Alloc(_) => Ok(true),
        _ => Err("Not an alloc value".to_string()),
    }
}
impl ModulePass for ToSSA {
    fn run_on(&mut self, program: &mut koopa::ir::Program) {
        let mut global_vars: HashSet<Value> = HashSet::new();
        for val in program.inst_layout() {
            global_vars.insert(*val);
        }
        let functions = program.funcs().keys().cloned().collect::<Vec<_>>();
        for func in functions {
            let func_name = program.func(func).name();
            println!("funcname: {}", func_name);
            let bbs = program.func(func).layout().bbs().keys().cloned().collect::<Vec<_>>();
            if bbs.is_empty() {
                continue;
            }
            let mut val_to_name: HashMap<Value, String> = HashMap::new();
            let mut name_to_val: HashMap<String, (Value, Type)> = HashMap::new();
            // 到达某个基本块时分配的所有变量
            let mut allocated_variables: HashMap<BasicBlock, HashSet<String>> = HashMap::new();
            // 某个基本块内部自己分配的变量
            let mut self_allocated: HashMap<BasicBlock, HashSet<String>> = HashMap::new();
            let mut alloc_inst = HashSet::new();
            let mut used_vals: HashMap<BasicBlock, HashSet<String>> = HashMap::new();
            let mut name_gen = NameGenerator::new();
            for bb in &bbs {
                let mut referenced_vals = HashSet::new();
                let vals = program.func(func).layout().bbs().node(bb).unwrap().insts().keys().cloned().collect::<Vec<_>>();
                let end_val = program.func(func).layout().bbs().node(bb).unwrap().insts().back_key().unwrap();
                for val in vals {
                    let value_data = program.func(func).dfg().value(val);
                    match value_data.kind() {
                        ValueKind::Alloc(_) => {
                            match value_data.ty().kind() {
                                TypeKind::Pointer(ptr) => {
                                    if TypeKind::Int32 == *ptr.kind() {
                                        let name = name_gen.get_name(value_data.name().as_ref().unwrap());
                                        val_to_name.insert(val, name.clone());
                                        name_to_val.insert(name.clone(), (val, ptr.clone()));
                                        allocated_variables.entry(*bb).or_default().insert(name.clone());
                                        self_allocated.entry(*bb).or_default().insert(name);
                                        alloc_inst.insert(val);
                                    }
                                },
                                _ => unreachable!(),
                            }
                        },
                        ValueKind::Store(store) => {
                            if let Some(name) = val_to_name.get(&store.dest()) {
                                referenced_vals.insert(name.clone());
                            }
                        },
                        ValueKind::Load(load) => {
                            if let Some(name) = val_to_name.get(&load.src()) {
                                referenced_vals.insert(name.clone());
                            }
                        }
                        _ => {}
                    }
                }
                used_vals.insert(*bb, referenced_vals);
                let end_data = program.func(func).dfg().value(*end_val);
                let cur_vars = allocated_variables.entry(*bb).or_default().clone();
                match end_data.kind() {
                    ValueKind::Jump(jump) => {
                        let bb = jump.target();
                        allocated_variables.entry(bb).or_default().extend(cur_vars);
                    },
                    ValueKind::Branch(branch) => {
                        let true_bb = branch.true_bb();
                        let false_bb = branch.false_bb();
                        allocated_variables.entry(true_bb).or_default().extend(cur_vars.clone());
                        allocated_variables.entry(false_bb).or_default().extend(cur_vars);
                    },
                    _ => {}
                };
            }

            let (pred, end_bb) = get_pred_and_end(program, func);
            let order = get_topo_order(&end_bb, &pred);
            let (active_at_entry, _) = get_active_values(program, func, &order, &global_vars, is_alloc);
            let mut changed = true;
            while changed {
                changed = false;
                for bb in order.iter() {
                    let pred_bbs = match pred.get(bb) {
                        Some(pred) => pred,
                        None => continue
                    };
                    let old_size = match allocated_variables.get(bb) {
                        Some(vars) => vars.len(),
                        None => 0
                    };
                    for pred in pred_bbs.iter() {
                        let pre_vars = match allocated_variables.get(pred) {
                            Some(pre_vars) => pre_vars.clone(),
                            None => HashSet::new()
                        };
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

            // 从allocated_variables里删去self_allocated的变量
            // 添加基本块参数的时候只添加在其他基本块里分配而这个基本块里可能用到的变量
            for bb in &bbs {
                if let Some(all_vars) =  allocated_variables.get_mut(bb) {
                    if let Some(self_vars) = self_allocated.get(bb) {
                        for var in self_vars {
                            all_vars.remove(var);
                        }
                    }
                    if let Some(active_vals) = active_at_entry.get(bb) {
                        all_vars.retain(|name| {
                            let val = name_to_val.get(name).unwrap().0;
                            return active_vals.contains(&val);
                        });
                    }
                }
            }

            let mut env = Environment::new();
            let mut delete_bb = HashSet::new();
            let mut bb_map: HashMap<BasicBlock, BasicBlock> = HashMap::new();
            for bb in &bbs {
                let name = program.func(func).dfg().bb(*bb).name().clone();
                let params = match allocated_variables.get(bb) {
                    Some(vars) => vars.clone(),
                    None => HashSet::new(),
                };
                delete_bb.insert(*bb);
                let params = params.iter().enumerate().map(|(i, str)| {
                    let (val, ty) = name_to_val.get(str).unwrap();
                    print!("{} ", str);
                    ty.clone()
                }).collect::<Vec<_>>();
                let new_bb = program.func_mut(func).dfg_mut().new_bb().basic_block_with_params(name.clone(), params.clone());
                println!("name: {} newbb: {:?}, old_bb: {:?}, param_len: {}", name.unwrap(), new_bb, bb, params.len());
                program.func_mut(func).layout_mut().bbs_mut().push_key_back(new_bb).unwrap();
                bb_map.insert(*bb, new_bb);
            }

            let mut val_map: HashMap<Value, Value> = HashMap::new();
            for bb in &bbs {
                println!("processing bb: {:?}", bb);
                let params = match allocated_variables.get(bb) {
                    Some(vars) => vars.clone(),
                    None => HashSet::new(),
                };
                let mut index_map: HashMap<usize, Value> = HashMap::new();
                if let Some(self_vars) = self_allocated.get(bb) {
                    for var in self_vars {
                        let val = name_to_val.get(var).unwrap().0;
                        env.insert_old(val, None);
                    }
                }
                for (i, str) in params.iter().enumerate() {
                    let (val, _) = name_to_val.get(str).unwrap();
                    index_map.insert(i, *val);
                }
                let new_bb = *bb_map.get(bb).unwrap();
                {
                    let new_bb = program.func_mut(func).dfg_mut().bb_mut(new_bb);
                    for (i, val) in new_bb.params().iter().enumerate() {
                        println!("param {}: {:?}", i, val);
                        env.insert_new(*index_map.get(&i).unwrap(), *val);
                    }
                }

                let mut new_insts = Vec::new();
                let zero_val = program.func_mut(func).dfg_mut().new_value().integer(0);

                let get_new_bb_target = |bb: BasicBlock, _: &[Value], env: &Environment, _: &HashMap<Value, Value>, changed: &mut bool| -> (BasicBlock, Vec<Value>){
                    match bb_map.get(&bb) {
                        Some(new_bb) => {
                            *changed = true;
                            println!("new_bb: {:?}", new_bb);
                            let params = match allocated_variables.get(&bb) {
                                Some(vars) => {
                                    vars.iter().map(|param| {
                                        let (val, _) = name_to_val.get(param).unwrap();
                                        let res = env.get(val).unwrap_or_else(|| zero_val);
                                        println!("arg name: {}, val: {:?}", param, res);
                                        res
                                    }).collect::<Vec<_>>()
                                },
                                None => vec![],
                            };
                            (*new_bb, params)
                        },
                        None => (bb, vec![])
                    }
                };
                let vals = program.func(func).layout().bbs().node(bb).unwrap().insts().keys().cloned().collect::<Vec<_>>();
                for val in vals {
                    if alloc_inst.contains(&val) {
                        continue;
                    }
                    let value_data = program.func(func).dfg().value(val).clone();
                    match value_data.kind() {
                        ValueKind::Store(store) => {
                            let new_val = store.value();
                            let new_val = match val_map.get(&new_val) {
                                Some(v) => *v,
                                None => new_val
                            };
                            let dest = store.dest();
                            if val_to_name.contains_key(&dest) {
                                let name = val_to_name.get(&dest).unwrap();
                                println!("store val: {:?} to {}", new_val, name);
                                env.set(dest, new_val);
                            } else {
                                process_inst(program.func_mut(func), val, &mut val_map, &mut new_insts, &get_new_bb_target, &env, true);
                            }
                        },
                        ValueKind::Load(load) => {
                            let src = load.src();
                            if let Some(real_val) = env.get(&src) {
                                val_map.insert(val, real_val);
                            } else {
                                process_inst(program.func_mut(func), val, &mut val_map, &mut new_insts, &get_new_bb_target, &env, true);
                            }
                        },
                        _ => {
                            process_inst(program.func_mut(func), val, &mut val_map, &mut new_insts, &get_new_bb_target, &env, true);
                        }
                    }
                }
                program.func_mut(func).layout_mut().bb_mut(new_bb).insts_mut().extend(new_insts);
            }

            delete_bbs(program.func_mut(func), &delete_bb);
            let mut dce = DeadCodeElimination::new();
            dce.run_on(program);

            let mut active_bbs = HashSet::new();
            for bb in program.func(func).layout().bbs().keys() {
                active_bbs.insert(*bb);
                assert!(!delete_bb.contains(bb));
            }
            let mut val_map = HashMap::new();
            changed = true;
            while changed {
                changed = false;
                delete_bb.clear();
                let bbs = program.func(func).layout().bbs().keys().cloned().collect::<Vec<_>>();
                for &cur_bb in bbs.iter() {
                    let bb_data = program.func(func).dfg().bb(cur_bb);
                    let bb_name = bb_data.name().as_ref().unwrap().clone();
                    println!("bb_name: {}, bb: {:?}", bb_name, cur_bb);

                    // let used_by = bb_data.used_by().clone();
                    let params = Vec::from(bb_data.params());
                    let param_num = params.len();
                    let mut actual_param = vec![None; param_num];
                    let mut removable = vec![true; param_num];
                    let mut all_vals = HashSet::new();
                    for bb in active_bbs.iter() {
                        let vals = program.func(func).layout().bbs().node(bb).unwrap().insts().keys().cloned().collect::<Vec<_>>();
                        for val in vals {
                            all_vals.insert(val);
                        }
                    }
                    for val in all_vals.iter() {
                        let value_data = program.func(func).dfg().value(*val);
                        let mut process = |bb: BasicBlock, vals: &[Value]| {
                            for i in 0..param_num {
                                if let Some(val_) = vals.get(i) {
                                    if *val_ != params[i] {
                                        match actual_param[i] {
                                            Some(param) => {
                                                if param != *val_ {
                                                    removable[i] = false;
                                                }
                                            },
                                            None => {
                                                actual_param[i] = Some(*val_);
                                            }
                                        }
                                    }
                                } else {
                                    unreachable!()
                                }
                            }
                        };
                        match value_data.kind() {
                            ValueKind::Branch(branch) => {
                                let true_bb = branch.true_bb();
                                if true_bb == cur_bb {
                                    process(true_bb, branch.true_args());
                                }
                                let false_bb = branch.false_bb();
                                if false_bb == cur_bb {
                                    process(false_bb, branch.false_args());
                                }
                            },
                            ValueKind::Jump(jump) => {
                                let target = jump.target();
                                if target == cur_bb {
                                    process(target, jump.args());
                                }
                            },
                            _ => {}
                        }
                    }
                    let mut params_types = Vec::new();
                    let mut flag = false;
                    for i in 0..param_num {
                        let val = params[i];
                        if removable[i] {
                            flag = true;
                            let replace_val = match actual_param[i] {
                                Some(val) => val,
                                None => {flag = false; break;}
                            };
                            val_map.insert(val, replace_val);
                        } else {
                            let val_data = program.func(func).dfg().value(val);
                            params_types.push(val_data.ty().clone());
                        }
                    }
                    if !flag {
                        continue;
                    }
                    changed = true;
                    delete_bb.insert(cur_bb);
                    active_bbs.remove(&cur_bb);
                    let new_bb = program.func_mut(func).dfg_mut().new_bb().basic_block_with_params(Some(bb_name), params_types);
                    println!("new_bb: {:?}", new_bb);
                    program.func_mut(func).layout_mut().bbs_mut().push_key_back(new_bb).unwrap();
                    let new_params = program.func(func).dfg().bb(new_bb).params();
                    active_bbs.insert(new_bb);
                    let mut cnt = 0;
                    for i in 0..param_num {
                        if !removable[i] {
                            let new_val = new_params[cnt];
                            cnt += 1;
                            val_map.insert(params[i], new_val);
                        }
                    }

                    let get_new_bb_target = |bb: BasicBlock, params: &[Value], _: &Environment, val_map: &HashMap<Value, Value>, changed: &mut bool| -> (BasicBlock, Vec<Value>) {
                        if bb == cur_bb {
                            *changed = true;
                            let new_params = params.iter().enumerate().filter_map(|(i, val)| {
                                if !removable[i] {
                                    let val = val_map.get(val).unwrap_or_else(|| val);
                                    Some(val)
                                } else {
                                    None
                                }
                            }).cloned().collect::<Vec<_>>();
                            (new_bb, new_params)
                        } else {
                            let new_params = params.iter().map(|val| {
                                match val_map.get(val) {
                                    Some(val) => {
                                        *changed = true;
                                        *val
                                    },
                                    None => *val
                                }
                            }).collect::<Vec<_>>();
                            (bb, new_params)
                        }
                    };
                    let mut new_insts = Vec::new();
                    let vals = program.func(func).layout().bbs().node(&cur_bb).unwrap().insts().keys().cloned().collect::<Vec<_>>();
                    for val in vals {
                        process_inst(program.func_mut(func), val, &mut val_map, &mut new_insts, &get_new_bb_target, &env, false);
                    }
                    program.func_mut(func).layout_mut().bb_mut(new_bb).insts_mut().extend(new_insts);

                    for &bb in active_bbs.iter() {
                        if bb == cur_bb {
                            continue;
                        }
                        let vals = program.func(func).layout().bbs().node(&bb).unwrap().insts().keys().cloned().collect::<Vec<_>>();
                        for val in vals {
                            replace_inst(program.func_mut(func), val, &mut val_map, &get_new_bb_target);
                        }
                    }

                }
                delete_bbs(program.func_mut(func), &delete_bb);
            }
        }
    }
}