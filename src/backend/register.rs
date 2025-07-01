use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use koopa::ir::{BasicBlock, Program, ValueKind, Function, Value};
use koopa::ir::entities::ValueData;
use crate::backend::environment::Environment;

const FREE_REG: usize = 25;
pub fn get_pred_and_end(program: &Program, func: Function) ->
               (HashMap<BasicBlock, Vec<BasicBlock>>, Vec<BasicBlock>) {
    let func_data = program.func(func);
    let bbs = func_data.layout().bbs().keys().cloned().collect::<Vec<_>>();
    let mut end_bb = Vec::new();
    let mut pred: HashMap<BasicBlock, Vec<BasicBlock>> = HashMap::new();
    for bb in &bbs {
        let bb_node = func_data.layout().bbs().node(bb).unwrap();
        let end = bb_node.insts().back_key().unwrap();
        let end = func_data.dfg().value(*end);
        match end.kind() {
            ValueKind::Branch(branch) => {
                let tr = branch.true_bb();
                let fa = branch.false_bb();
                pred.entry(tr).or_default().push(*bb);
                pred.entry(fa).or_default().push(*bb);
            }
            ValueKind::Jump(jump) => {
                let target = jump.target();
                pred.entry(target).or_default().push(*bb);
            }
            ValueKind::Return(_) => {
                end_bb.push(*bb);
            }
            _ => {
                // Other kinds of instructions do not affect the predecessors
            }
        };
    }
    (pred, end_bb)
}
pub fn get_topo_order(
    end_bb: &Vec<BasicBlock>,
    pred: &HashMap<BasicBlock, Vec<BasicBlock>>,
) -> Vec<BasicBlock> {
    let mut visited: HashSet<BasicBlock> = HashSet::new();
    let mut stack = Vec::new();
    let mut order = Vec::new();

    for bb in end_bb {
        if !visited.contains(&bb) {
            stack.push(*bb);
            while let Some(current) = stack.pop() {
                if !visited.insert(current) {
                    continue;
                }
                order.push(current);
                if let Some(predecessors) = pred.get(&current) {
                    for pred_bb in predecessors {
                        if !visited.contains(&pred_bb) {
                            stack.push(*pred_bb);
                        }
                    }
                }
            }
        }
    }
    order
}
fn get_referenced_value(val: &ValueData) -> Vec<Value> {
    match val.kind() {
        ValueKind::Branch(branch) => {
            let mut res = Vec::new();
            let cond = branch.cond();
            res.push(cond);
            res.extend(branch.true_args());
            res.extend(branch.false_args());
            res
        },
        ValueKind::Jump(jump) => {
            Vec::from(jump.args())
        },
        ValueKind::Binary(binary) => {
            vec![binary.lhs(), binary.rhs()]
        },
        ValueKind::Store(store) => {
            vec![store.value(), store.dest()]
        },
        ValueKind::Call(call) => {
            call.args().iter().map(|arg| *arg).collect()
        },
        ValueKind::Load(load) => {
            vec![load.src()]
        }
        ValueKind::Return(ret) => {
            if let Some(value) = ret.value() {
                vec![value]
            } else {
                Vec::new()
            }
        }
        ValueKind::GetPtr(getptr) => {
            vec![getptr.src(), getptr.index()]
        },
        ValueKind::GetElemPtr(getelemptr) => {
            vec![getelemptr.index()]
        },
        _ => Vec::new(),
    }
}

fn need_register_allocation(val: &ValueData) -> Result<bool, String> {
    match val.kind() {
        ValueKind::Binary(_) => Ok(true),
        ValueKind::Alloc(_) => Ok(false),
        ValueKind::GlobalAlloc(_) => Ok(false),
        ValueKind::Load(_) => Ok(true),
        ValueKind::GetElemPtr(_) => Ok(true),
        ValueKind::GetPtr(_) => Ok(true),
        ValueKind::Integer(_) => Ok(true),
        ValueKind::Call(_) => Ok(true),
        ValueKind::BlockArgRef(_) => Ok(true),
        ValueKind::FuncArgRef(_) => Ok(true),
        _ => Err(format!("Unexpected value kind: {:?}", val.kind())),
    }
}
pub type Register = u32;
pub fn to_string(reg: Register) -> String {
    if reg < 12 {
        format!("s{}", reg)
    } else if reg < 19 {
        format!("t{}", reg - 12)
    } else if reg < 27 {
        format!("a{}", reg - 19)
    } else {
        panic!("Register out of range: {}", reg);
    }
}
pub const A0: Register = 19;
pub const A6: Register = 25;
pub const A7: Register = 26;
pub fn from_string(s: &str) -> Result<Register, String> {
    if s.starts_with("s") {
        let reg_num = s[1..].parse::<u32>().map_err(|_| format!("Invalid register name: {}", s))?;
        if reg_num < 12 {
            Ok(reg_num)
        } else {
            Err(format!("Invalid s-register number: {}", reg_num))
        }
    } else if s.starts_with("t") {
        let reg_num = s[1..].parse::<u32>().map_err(|_| format!("Invalid register name: {}", s))?;
        if reg_num < 7 {
            Ok(reg_num + 12)
        } else {
            Err(format!("Invalid t-register number: {}", reg_num))
        }
    } else if s.starts_with("a") {
        let reg_num = s[1..].parse::<u32>().map_err(|_| format!("Invalid register name: {}", s))?;
        if reg_num < 7 {
            Ok(reg_num + 19)
        } else {
            Err(format!("Invalid a-register number: {}", reg_num))
        }
    } else {
        Err(format!("Unknown register prefix in name: {}", s))
    }
}
struct Node {
    val: Value,
    weight: usize,
}
impl Node {
    fn new(val: Value, weight: usize) -> Self {
        Node { val, weight }
    }
}
impl Eq for Node {}
impl PartialEq<Self> for Node {
    fn eq(&self, other: &Self) -> bool {
        self.weight == other.weight
    }
}

impl PartialOrd<Self> for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.weight.cmp(&self.weight)
    }
}
fn alloc_register(
    reg_map: &mut HashMap<Value, Register>,
    val: Value,
    conflicts: &Vec<Value>,
) -> Register {
    let mut free_regs = vec![true; FREE_REG];
    for conflict in conflicts {
        if val == *conflict {
            continue;
        }
        if let Some(&reg) = reg_map.get(conflict) {
            if reg < FREE_REG as u32 {
                free_regs[reg as usize] = false;
            } else {
                panic!("Register out of range: {}", reg);
            }
        }
    }
    for i in 0..FREE_REG {
        if free_regs[i] {
            reg_map.insert(val, i as Register);
            return i as Register;
        }
    }
    panic!("No free registers available for value: {:?}", val);
}
pub fn get_active_values(program: &Program, func: Function, order: &Vec<BasicBlock>, exclude: &HashSet<Value>, 
                         should_consider:  fn(&ValueData) -> Result<bool, String>) -> 
(HashMap<BasicBlock, HashSet<Value>>, HashMap<Value, HashSet<Value>>) {

    let mut active_val: HashMap<Value, HashSet<Value>> = HashMap::new();
    let mut active_at_entry: HashMap<BasicBlock, HashSet<Value>> = HashMap::new();
    let mut changed = true;
    while changed {
        changed = false;
        for bb in order.iter() {
            let bb_node = program.func(func).layout().bbs().node(bb).unwrap();
            let end = bb_node.insts().back_key().unwrap();
            let end_val = program.func(func).dfg().value(*end);
            let mut merge_bb = |bb: &BasicBlock| {
                let end_active = active_val.entry(*end).or_default();
                let start_active = active_at_entry.entry(*bb).or_default();
                let old_size = end_active.len();
                end_active.extend(start_active.iter());
                let new_size = end_active.len();
                if old_size != new_size {
                    changed = true;
                }
            };
            match end_val.kind() {
                ValueKind::Branch(branch) => {
                    let tr = branch.true_bb();
                    let fa = branch.false_bb();
                    merge_bb(&tr);
                    merge_bb(&fa);
                }
                ValueKind::Jump(jump) => {
                    let target = jump.target();
                    merge_bb(&target);
                }
                _ => {}
            }

            let vals = bb_node.insts().keys().cloned().collect::<Vec<_>>();
            let get_active = |next: Value, active_val: &mut HashMap<Value, HashSet<Value>>| -> HashSet<Value> {
                let next_val = program.func(func).dfg().value(next);
                let mut active = match active_val.get(&next) {
                    Some(active) => active.clone(),
                    None => {
                        active_val.insert(next, HashSet::new());
                        HashSet::new()
                    }
                };
                let used = get_referenced_value(&next_val);
                for used_val in used {
                    if exclude.contains(&used_val) {
                        continue;
                    }
                    let used_val_data = program.func(func).dfg().value(used_val);
                    let store = match should_consider(used_val_data) {
                        Ok(true) => true,
                        Ok(false) => false,
                        Err(_) => false,
                    };
                    if store {
                        active.insert(used_val);
                    }
                }
                active.remove(&next);
                active
            };
            for i in (0..vals.len() - 1).rev() {
                let val = vals[i];
                let next = vals[i + 1];
                let active = get_active(next, &mut active_val);
                active_val.insert(val, active);
            }
            let first_val = *vals.first().unwrap();
            let mut active = get_active(first_val, &mut active_val);
            let bb_data = program.func(func).dfg().bb(*bb);
            for arg in bb_data.params() {
                active.remove(&arg);
            }
            active_at_entry.insert(*bb, active);
        }
    }
    (active_at_entry, active_val)
}
fn process_conflicts_at_entry(active: &HashSet<Value>, params: &[Value], conflicts: &mut HashMap<Value, Vec<Value>>) {
    for val in active {
        conflicts.entry(*val).or_default().extend(params);
    }
    for val in params {
        conflicts.entry(*val).or_default().extend(active);
    }
    for param in params {
        conflicts.entry(*param).or_default().extend(params.iter().cloned());
    }
}
pub fn get_register_map(env: &Environment, program: &Program, func: Function) -> (HashMap<Value, Register>, u32) {
    let (pred, end_bb) = get_pred_and_end(program, func);
    let order = get_topo_order(&end_bb, &pred);
    let bbs = program.func(func).layout().bbs().keys().collect::<Vec<_>>();
    assert_eq!(order.len(), bbs.len());

    let (active_at_entry, active_val) = get_active_values(program, func, &order, &env.global_symbol, need_register_allocation);

    let mut weight: HashMap<Value, usize> = HashMap::new();
    let mut conflicts: HashMap<Value, Vec<Value>> = HashMap::new();
    let mut vals: Vec<Value> = Vec::from(program.func(func).params());
    
    let func_data = program.func(func);
    let params = func_data.params();
    let entry_bb = func_data.layout().entry_bb().unwrap();
    if let Some(active) = active_at_entry.get(&entry_bb) {
        process_conflicts_at_entry(active, &params, &mut conflicts);
    }
    
    for bb in order.iter() {
        let bb_node = program.func(func).layout().bbs().node(bb).unwrap();
        vals.extend(bb_node.insts().keys().cloned());
        let bb_data = program.func(func).dfg().bb(*bb);
        vals.extend(bb_data.params());
        for val in bb_node.insts().keys() {
            let val_data = program.func(func).dfg().value(*val);
            vals.extend(get_referenced_value(val_data));
        }
        if let Some(active) = active_at_entry.get(bb) {
            process_conflicts_at_entry(active, bb_data.params(), &mut conflicts);
        }
    }
    for val in vals {
        if env.global_symbol.contains(&val) {
            continue;
        }
        let val_data = program.func(func).dfg().value(val);
        match need_register_allocation(val_data) {
            Ok(false) => continue,
            Ok(true) => {},
            Err(_) => continue
        };
        weight.insert(val, 0);
        if !active_val.contains_key(&val) {
            continue;
        }
        let active = active_val.get(&val).unwrap();
        if active.is_empty() {
            continue;
        }
        for con_val in active {
            conflicts.entry(*con_val).or_default().push(val);
            conflicts.entry(val).or_default().push(*con_val);
        }
    }
    let mut max_reg = 0;
    let mut priority_queue: BinaryHeap<Node> = BinaryHeap::new();
    let mut reg_map: HashMap<Value, Register> = HashMap::new();

    let vals = weight.keys().cloned().collect::<Vec<_>>();
    for val in vals {
        if reg_map.contains_key(&val) {
            continue;
        }
        priority_queue.push(Node::new(val, weight.get(&val).unwrap().clone()));
        while let Some(node) = priority_queue.pop() {
            let val = node.val;
            if reg_map.contains_key(&val) {
                continue;
            }
            if !conflicts.contains_key(&val) {
                // If there are no conflicts, allocate a register directly
                reg_map.insert(val, 0);
                continue;
            }
            let conflicts_list = conflicts.get(&val).unwrap();
            // Allocate register for the value
            let reg = alloc_register(&mut reg_map, val, conflicts_list);
            if reg > max_reg {
                max_reg = reg;
            }
            for conflict in conflicts_list {
                if !reg_map.contains_key(conflict) {
                    let conflict_weight = *weight.get(conflict).unwrap() + 1;
                    weight.insert(*conflict, conflict_weight);
                    priority_queue.push(Node::new(*conflict, conflict_weight));
                }
            }
        }

    }
    (reg_map, max_reg + 1)
}

