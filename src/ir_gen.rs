use std::rc::Rc;
use koopa::ir::{BasicBlock, FunctionData, Type, Value};
use koopa::ir;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use crate::ast;
use crate::ast::*;
use crate::environment::*;
use crate::sym_table::{SymbolEntry};

impl IRGen for CompUnit {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        for comp in self.items.iter() {
            comp.generate_ir(env)?;
        }
        Ok(())
    }
}
impl IRGen for Comp {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        match self {
            Comp::FuncDef(func_def) => func_def.generate_ir(env),
            Comp::Decl(decl) => decl.generate_ir(env),
        }
    }
}
impl IRGen for FuncDef {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        let func = env.program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident),
            self.get_param(),
            match self.func_type {
                FuncType::Void => Type::get_unit(),
                FuncType::Int => Type::get_i32(),
            },
        ));
        env.cur_func = Some(func);
        let old_table = env.enter_scope();
        if let Some(params) = &*self.params {
            params.add_params(env)?;
        }
        let entry= env.create_block("entry");
        env.cur_bb = Some(entry);
        self.block.generate_ir(env)?;
        env.exit_scope(old_table);
        env.sym_table.borrow_mut().insert_func(self.ident.clone(), func)?;
        Ok(())
    }
}

impl IRGen for Block {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        for item in self.items.iter() {
            item.generate_ir(env)?;
        }
        Ok(())
    }
}
impl IRGen for BlockItem {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        match self {
            BlockItem::Stmt(stmt) => stmt.generate_ir(env)?,
            BlockItem::Decl(decl) => decl.generate_ir(env)?,
        }
        Ok(())
    }
}
impl IRGen for Decl {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_ir(env)?,
            Decl::VarDecl(var_decl) => var_decl.generate_ir(env)?,
        }
        Ok(())
    }
}
impl IRGen for VarDecl {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        for var_def in self.var_defs.iter() {
            match var_def {
                VarDef::Def(ident) => {
                    let val = match env.is_global() {
                        true => {
                            let init = env.program.new_value().zero_init(self.btype.to_type());
                            env.add_global_alloc(init, ident.clone())
                        },
                        false => {
                            env.add_alloc(self.btype.to_type())
                        },
                    };
                    env.sym_table.borrow_mut().insert_var(ident.clone(), val)?;
                }
                VarDef::Init(ident, exp) => {
                    let val = match env.is_global() {
                        true => {
                            let init = match exp.as_ref() {
                                InitVal::Exp(exp) => {
                                    let value = exp.eval_const(env)?;
                                    env.program.new_value().integer(value)
                                },
                            };
                            env.add_global_alloc(init, ident.clone())
                        },
                        false => {
                            let value = match exp.as_ref() {
                                InitVal::Exp(exp) => {
                                    exp.generate_ir(env)?
                                }
                            };
                            let pos = env.add_alloc(self.btype.to_type());
                            env.add_store(value, pos);
                            pos
                        },
                    };
                    env.sym_table.borrow_mut().insert_var(ident.clone(), val)?;
                },
            }
        }
        Ok(())
    }
}

impl IRGen for ConstDecl {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        for const_def in self.const_defs.iter() {
            let init_val = match const_def.const_init_val.as_ref() {
                ConstInitVal::ConstExp(exp) => {
                    exp.eval_const(env)?
                }
            };
            // let pos = match env.is_global() {
            //     true => {
            //         let init_val = env.program.new_value().integer(init_val);
            //         env.add_global_alloc(init_val, const_def.ident.clone())
            //     },
            //     false => {
            //         let init_val = env.add_integer(init_val);
            //         let pos = env.add_alloc(self.btype.to_type());
            //         env.add_store(init_val, pos);
            //         pos
            //     }
            // };
            env.sym_table.borrow_mut().insert_const(const_def.ident.clone(), init_val)?;
        }
        Ok(())
    }
}
impl Stmt {
    fn gen_single_block(&self, env: &mut Environment, bb: BasicBlock, stmt: &Rc<Stmt>, target_bb: BasicBlock) -> Result<(), FrontendError> {
        env.cur_bb = Some(bb);
        let old_table = env.enter_scope();
        stmt.generate_ir(env)?;
        env.add_jump(target_bb.clone());
        env.exit_scope(old_table);
        Ok(())
    }
    fn generate_ir(&self, env: &mut Environment) -> Result<(), FrontendError> {
        match self {
            Stmt::Exp(exp) => {
                if let Some(exp) = exp.as_ref() {
                    let _ = exp.generate_ir(env)?;
                }
                Ok(())
            },
            Stmt::Block(block) => {
                let old_table = env.enter_scope();
                block.generate_ir(env)?;
                env.exit_scope(old_table);
                Ok(())
            },
            Stmt::Return(exp) => {
                if let Some(exp) = exp.as_ref() {
                    let value = exp.generate_ir(env)?;
                    env.add_ret(Some(value));
                } else {
                    env.add_ret(None);
                }
                Ok(())
            },
            Stmt::If(exp, then, else_) => {
                let cond = exp.generate_ir(env)?;
                let then_bb = env.create_block("then");
                let end_bb = env.create_block("end");
                match else_ {
                    Some(else_stmt) => {
                        let else_bb = env.create_block("else");

                        env.add_branch(cond, then_bb, else_bb);

                        self.gen_single_block(env, then_bb, then, end_bb)?;
                        self.gen_single_block(env, else_bb, else_stmt, end_bb)?;
                    },
                    None => {
                        env.add_branch(cond, then_bb, end_bb.clone());

                        self.gen_single_block(env, then_bb, then, end_bb)?;
                    },
                }
                env.cur_bb = Some(end_bb);
                Ok(())
            },
            Stmt::While(exp, stmt) => {
                let start_bb = env.create_block("while_start");
                let body_bb = env.create_block("while_body");
                let end_bb = env.create_block("while_end");

                let old_while_env = env.while_env.clone();
                env.while_env = Some(WhileEnv {
                    start: start_bb,
                    next: end_bb,
                });
                env.add_jump(start_bb.clone());
                env.cur_bb = Some(start_bb);
                let cond = exp.generate_ir(env)?;
                env.add_branch(cond, body_bb.clone(), end_bb.clone());

                self.gen_single_block(env, body_bb, stmt, end_bb)?;
                env.while_env = old_while_env;

                env.cur_bb = Some(end_bb);
                Ok(())
            },
            Stmt::Continue => {
                match &env.while_env {
                    Some(while_env) => {
                        env.add_jump(while_env.start.clone());
                    },
                    None => return Err(FrontendError::ContinueOutsideLoop),
                }
                Ok(())
            },
            Stmt::Break => {
                match &env.while_env {
                    Some(while_env) => {
                        env.add_jump(while_env.next.clone());
                    },
                    None => return Err(FrontendError::BreakOutsideLoop),
                }
                Ok(())
            },
            Stmt::Assign(lval, exp) => {
                let value = exp.generate_ir(env)?;
                let name = &lval.ident;
                let entry =  env.sym_table.borrow().get(name);
                match entry {
                    Some(entry) => {
                        match entry {
                            SymbolEntry::Var(var) => {
                                env.add_store(value, var);
                            },
                            _ => return Err(FrontendError::InvalidAssignment(name.clone())),
                        }
                    },
                    None => return Err(FrontendError::UndefinedVariable(name.clone())),
                }
                Ok(())
            },
        }
    }
}

impl IRGen for Exp {
    type Output = Value;
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        match self {
            Exp::BinaryExp(op, lhs, rhs) => {
                match op {
                    ast::BinaryOp::Lor => {
                        let end_bb = env.create_block("lor_end");
                        let cond_bb = env.create_block("lor_cond");

                        let result = env.add_alloc(Type::get_i32());
                        let lhs_val = lhs.generate_ir(env)?;
                        let zero = env.add_integer(0);
                        let lhs_res = env.add_binary_inst(ir::BinaryOp::NotEq, lhs_val, zero);
                        env.add_store(lhs_res, result);
                        env.add_branch(lhs_res, end_bb, cond_bb);

                        env.cur_bb = Some(cond_bb);
                        let rhs_val = rhs.generate_ir(env)?;
                        let rhs_res = env.add_binary_inst(ir::BinaryOp::NotEq, rhs_val, zero);
                        env.add_store(rhs_res, result);
                        env.add_jump(end_bb);

                        env.cur_bb = Some(end_bb);
                        let res = env.add_load(result);
                        Ok(res)
                    },
                    ast::BinaryOp::Land => {
                        let end_bb = env.create_block("land_end");
                        let cond_bb = env.create_block("land_cond");

                        let result = env.add_alloc(Type::get_i32());
                        let lhs_val = lhs.generate_ir(env)?;
                        let zero = env.add_integer(0);
                        let lhs_res = env.add_binary_inst(ir::BinaryOp::NotEq, lhs_val, zero);
                        env.add_store(lhs_res, result);
                        env.add_branch(lhs_res, cond_bb, end_bb);

                        env.cur_bb = Some(cond_bb);
                        let rhs_val = rhs.generate_ir(env)?;
                        let rhs_res = env.add_binary_inst(ir::BinaryOp::NotEq, rhs_val, zero);
                        env.add_store(rhs_res, result);
                        env.add_jump(end_bb);

                        env.cur_bb = Some(end_bb);
                        let res = env.add_load(result);
                        Ok(res)
                    },
                    _ => {
                        let lhs_val = lhs.generate_ir(env)?;
                        let rhs_val = rhs.generate_ir(env)?;
                        let inst = env.add_binary_inst(op.to_koopa_op(), lhs_val, rhs_val);
                        Ok(inst)
                    }
                }
            },
            Exp::Num(number) => {
                let value = env.add_integer(*number);
                Ok(value)
            },
            Exp::LVal(lval) => {
                let entry = env.sym_table.borrow().get(&lval.ident);
                match entry {
                    Some(SymbolEntry::Var(var)) => {
                        let load_inst = env.add_load(var);
                        Ok(load_inst)
                    },
                    Some(SymbolEntry::Const(value)) => {
                        let const_value = env.add_integer(value);
                        Ok(const_value)
                    },
                    _ => Err(FrontendError::UndefinedVariable(lval.ident.clone())),
                }
            }
            Exp::UnaryExp(op, exp) => {
                let exp_val = exp.generate_ir(env)?;
                match op {
                    UnaryOp::Neg => {
                        let zero = env.add_integer(0);
                        let sub = env.add_binary_inst(ir::BinaryOp::Sub, zero, exp_val);
                        Ok(sub)
                    },
                    UnaryOp::Not => {
                        let zero = env.add_integer(0);
                        let eq = env.add_binary_inst(ir::BinaryOp::Eq, exp_val, zero);
                        Ok(eq)
                    },
                    UnaryOp::Pos => {
                        Ok(exp_val)
                    },
                }
            },
            Exp::Call(name, args) => {
                let func = env.sym_table.borrow().get(name)
                    .and_then(|entry| match entry {
                        SymbolEntry::Func(func) => Some(func),
                        _ => None,
                    })
                    .ok_or_else(|| FrontendError::UndefinedFunction(name.clone()))?;

                let mut arg_values = Vec::new();
                if let Some(args) = args.as_ref() {
                    for arg in args.iter() {
                        let arg_value = arg.generate_ir(env)?;
                        arg_values.push(arg_value);
                    }
                }

                let call_inst = env.program.func_mut(env.cur_func.unwrap())
                    .dfg_mut()
                    .new_value()
                    .call(func, arg_values);
                env.add_inst(call_inst);
                Ok(call_inst)
            },
        }
    }
}