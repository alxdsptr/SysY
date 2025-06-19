mod opt;
mod frontend;
mod backend;

use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::{Result, Write};
use koopa::back::KoopaGenerator;
use koopa::ir::Program;
use koopa::opt::{Pass, PassManager};
use frontend::environment::{Environment, IRGen};
use opt::dead_code_elimination::DeadCodeElimination;
use crate::backend::generate_asm;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    let mut program = Program::new();
    let mut env = Environment::new(&mut program);
    let mut output_file = std::fs::File::create(output)?;
    match ast.generate_ir(&mut env){
        Ok(_) => {
            let mut passman = PassManager::new();
            passman.register(Pass::Module(Box::new(DeadCodeElimination)));
            passman.run_passes(&mut program);

            match mode.as_str() {
                "koopa" => {
                    let mut gen = KoopaGenerator::new(Vec::new());
                    gen.generate_on(&program)?;
                    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
                    println!("dump IR to file");
                    output_file.write_all(text_form_ir.as_bytes())?;
                },
                "riscv" => {

                    let mut gen = KoopaGenerator::new(Vec::new());
                    gen.generate_on(&program)?;
                    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
                    println!("dump IR to file");
                    let mut ir_file = std::fs::File::create("./ir.koopa")?;
                    ir_file.write_all(text_form_ir.as_bytes())?;

                    generate_asm(&program, &mut output_file);
                },
                _ => {
                    eprintln!("Unknown mode: {}", mode);
                    return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Unknown mode"));
                }
            }
        },
        Err(e) => {
            eprintln!("Error during IR generation: {:?}", e);
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "IR generation failed"));
        }
    }

    Ok(())
}
