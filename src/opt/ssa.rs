use std::collections::HashMap;
use koopa::ir::{BasicBlock, FunctionData};
use koopa::opt::ModulePass;
use koopa::ir::ValueKind;

pub struct ToSSA;

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
            let func_data = program.func(func);
            let bbs = func_data.layout().bbs().keys().cloned().collect::<Vec<_>>();
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
                    _ => {
                        // Other kinds of instructions do not affect the predecessors
                    }
                };
            }

        }


    }
}