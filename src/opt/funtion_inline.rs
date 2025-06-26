use koopa::ir::Program;
use koopa::opt::ModulePass;

pub struct FunctionInliner;

impl ModulePass for FunctionInliner {
    fn run_on(&mut self, program: &mut Program) {
        let mut inlinable = Vec::new();
        for func in program.func_layout() {
            let func_data = program.func(*func);
        }
    }
}