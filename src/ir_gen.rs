use std::cell::RefCell;
use std::collections::VecDeque;
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
        env.add_decl("getint", Vec::new(), Type::get_i32())?;
        env.add_decl("getch", Vec::new(), Type::get_i32())?;
        env.add_decl("getarray", vec![Type::get_pointer(Type::get_i32())], Type::get_i32())?;
        env.add_decl("putint", vec![Type::get_i32()], Type::get_unit())?;
        env.add_decl("putch", vec![Type::get_i32()], Type::get_unit())?;
        env.add_decl("putarray", vec![Type::get_i32(), Type::get_pointer(Type::get_i32())], Type::get_unit())?;
        env.add_decl("starttime", Vec::new(), Type::get_unit())?;
        env.add_decl("stoptime", Vec::new(), Type::get_unit())?;
        
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
        env.sym_table.borrow_mut().insert_func(self.ident.clone(), func)?;
        let old_table = env.enter_scope();
        if let Some(params) = &*self.params {
            params.add_params(env)?;
        }
        let entry= env.create_block("entry");
        env.cur_bb = Some(entry);
        self.block.generate_ir(env)?;
        env.exit_scope(old_table);
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
// pub fn dim_to_type(env: &mut Environment, base_type: Type, dims: &Vec<Exp>) -> Result<Type, FrontendError> {
//     let mut current_type = base_type;
//     for dim in dims.iter().rev() {
//         let dim_value = dim.eval_const(env)?;
//         if dim_value <= 0 {
//             return Err(FrontendError::InvalidArrayDim);
//         }
//         current_type = Type::get_array(current_type, dim_value as usize);
//     }
//     Ok(current_type)
// }
pub fn convert_dim(env: &mut Environment, array_dim: &Vec<Exp>) -> Result<(Vec<i32>, usize), FrontendError>{
    let mut raw_dim = Vec::with_capacity(array_dim.len());
    let mut total = 1;
    for dim in array_dim.iter() {
        let dim_value = dim.eval_const(env)?;
        if dim_value <= 0 {
            return Err(FrontendError::InvalidArrayDim);
        }
        raw_dim.push(dim_value as i32);
        total *= dim_value as usize;
    }
    Ok((raw_dim, total))
}
fn get_init_vals(env: &mut Environment, dim: &Vec<i32>, inits: Rc<RefCell<VecDeque<InitVal>>>, is_const: bool) -> Result<Vec<Value>, FrontendError> {
    let mut res = Vec::new();
    let mut cum_dim = dim.clone();
    for i in (0..dim.len() - 1).rev() {
        if cum_dim[i] <= 0 {
            return Err(FrontendError::InvalidArrayInitializer);
        }
        cum_dim[i] *= cum_dim[i + 1];
    }
    get_init_vals_helper(env, &cum_dim, inits, &mut res, is_const)?;
    Ok(res)
}
fn get_init_vals_helper(env: &mut Environment, dim: &[i32], inits: Rc<RefCell<VecDeque<InitVal>>>, res: &mut Vec<Value>, is_const: bool) -> Result<(), FrontendError>{
    let before = res.len() as i32;
    while !inits.borrow().is_empty() {
        let init = inits.borrow_mut().pop_front().unwrap();
        match init {
            InitVal::Exp(exp) => {
                let value = if is_const {
                    exp.generate_ir(env)?
                } else {
                    let res = exp.eval_const(env)?;
                    env.add_integer(res)
                };
                res.push(value);
            },
            InitVal::Array(inits_) => {
                if dim.is_empty() {
                    return Err(FrontendError::InvalidArrayInitializer);
                }
                let len = res.len() as i32;
                if len % dim.last().unwrap() != 0 {
                    return Err(FrontendError::InvalidArrayInitializer);
                } 
                for (i, d) in dim.iter().enumerate().skip(1) {
                    if len % d == 0 {
                        get_init_vals_helper(env, &dim[i..], inits_, res, is_const)?;
                        break;
                    }
                }
            },
        }
    }
    let after = res.len() as i32;
    let diff = after - before;
    for _ in diff..dim[0] {
        res.push(env.add_integer(0));
    }
    Ok(())
}
impl IRGen for VarDecl {
    type Output = ();
    fn generate_ir(&self, env: &mut Environment) -> Result<Self::Output, FrontendError> {
        for var_def in self.var_defs.iter() {
            match var_def {
                VarDef::Def(ident, array_dim) => {
                    let (raw_dim, total) = convert_dim(env, array_dim)?;
                    let ty = match array_dim.is_empty() {
                        true => self.btype.to_type(),
                        false => Type::get_array(self.btype.to_type(), total)
                    };
                    let val = match env.is_global() {
                        true => {
                            let init = env.program.new_value().zero_init(ty);
                            env.add_global_alloc(init, ident.clone())
                        },
                        false => {
                            env.add_alloc(ty)
                        },
                    };
                    match array_dim.is_empty() {
                        true => env.sym_table.borrow_mut().insert_var(ident.clone(), val, false)?,
                        false => env.sym_table.borrow_mut().insert_array(ident.clone(), val, Rc::new(raw_dim), self.btype.to_type(), false)?
                    }
                }
                VarDef::Init(ident, array_dim, exp) => {
                    let (raw_dim, total) = convert_dim(env, array_dim)?;
                    let val = match env.is_global() {
                        true => {
                            let init = match exp.as_ref() {
                                InitVal::Exp(exp) => {
                                    let value = exp.eval_const(env)?;
                                    env.program.new_value().integer(value)
                                },
                                InitVal::Array(inits) => {
                                    let res = get_init_vals(env, &raw_dim, inits.clone(), true)?;
                                    env.add_aggregate(res)
                                }
                            };
                            env.add_global_alloc(init, ident.clone())
                        },
                        false => {
                            let ty = match array_dim.is_empty() {
                                true => self.btype.to_type(),
                                false => Type::get_array(self.btype.to_type(), total)
                            };
                            let pos = env.add_alloc(ty);
                            match exp.as_ref() {
                                InitVal::Exp(exp) => {
                                    let value = exp.generate_ir(env)?;
                                    env.add_store(value, pos);
                                },
                                InitVal::Array(inits) => {
                                    let res = get_init_vals(env, &raw_dim, inits.clone(), true)?;
                                    for i in 0..total {
                                        let temp = env.add_integer(i as i32);
                                        let ptr = env.add_getelemptr(pos, temp);
                                        env.add_store(res[i], ptr);
                                    }
                                }
                            };
                            pos
                        },
                    };
                    match array_dim.is_empty() {
                        true => env.sym_table.borrow_mut().insert_var(ident.clone(), val, false)?,
                        false => env.sym_table.borrow_mut().insert_array(ident.clone(), val, Rc::new(raw_dim), self.btype.to_type(), false)?
                    }
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
            let (raw_dim, total) = convert_dim(env, &const_def.array_size)?;
            match const_def.const_init_val.as_ref() {
                InitVal::Exp(exp) => {
                    let init_val = exp.eval_const(env)?;
                    env.sym_table.borrow_mut().insert_const(const_def.ident.clone(), init_val)?;
                },
                InitVal::Array(inits) => {
                    let res = get_init_vals(env, &raw_dim, inits.clone(), true)?;
                    let init = env.add_aggregate(res);
                    let pos = match env.is_global() {
                        true => {
                            env.add_global_alloc(init, const_def.ident.clone())
                        },
                        false => {
                            let ty = Type::get_array(self.btype.to_type(), total);
                            let pos = env.add_alloc(ty);
                            env.add_store(init, pos);
                            pos
                        },
                    };
                    env.sym_table.borrow_mut().insert_array(const_def.ident.clone(), pos, Rc::new(raw_dim), self.btype.to_type(), true)?;
                }
            };
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

                self.gen_single_block(env, body_bb, stmt, start_bb)?;
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
                            SymbolEntry::Array(var, dims, _, is_const) => {
                                if is_const {
                                    return Err(FrontendError::InvalidAssignment(name.clone()));
                                }
                                let ptr = get_array_pos(env, var, lval, &dims, name.clone())?;
                                env.add_store(value, ptr);
                            },
                            SymbolEntry::ArrayPtr(var, dims) => {
                                let ptr = get_array_pos_from_ptr(env, var, lval, &dims)?;
                                env.add_store(value, ptr);
                            }
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
pub fn get_array_pos(env: &mut Environment, var: Value, lval: &LVal, dims: &Rc<Vec<i32>>, name: String) -> Result<Value, FrontendError> {
    let mut cum = 1;
    let mut res = env.add_integer(0);
    if dims.len() != lval.array_index.len() {
        return Err(FrontendError::ArrayIndexMismatch(name));
    }
    for i in (0..dims.len()).rev() {
        let temp = lval.array_index.get(i).unwrap().generate_ir(env)?;
        let integer = env.add_integer(cum);
        let temp = env.add_binary_inst(ir::BinaryOp::Mul, temp, integer);
        res = env.add_binary_inst(ir::BinaryOp::Add, res, temp);
        cum = cum * dims[i];
    }
    let ptr = env.add_getelemptr(var, res);
    Ok(ptr)
}
pub fn get_array_pos_from_ptr(env: &mut Environment, var: Value, lval: &LVal, dims: &Rc<Vec<i32>>) -> Result<Value, FrontendError> {
    let mut cum = 1;
    let mut res = env.add_integer(0);
    if dims.len() + 1 != lval.array_index.len() {
        return Err(FrontendError::ArrayIndexMismatch(lval.ident.clone()));
    }
    for i in (0..dims.len() + 1).rev() {
        let temp = lval.array_index.get(i).unwrap().generate_ir(env)?;
        let integer = env.add_integer(cum);
        let temp = env.add_binary_inst(ir::BinaryOp::Mul, temp, integer);
        res = env.add_binary_inst(ir::BinaryOp::Add, res, temp);
        if i < dims.len() {
            cum = cum * dims[i];
        }
    }
    let ptr = env.add_get_ptr(var, res);
    Ok(ptr)
}
pub fn get_array_ptr(env: &mut Environment, var: Value, lval: &LVal, dims: &Rc<Vec<i32>>) -> Result<Value, FrontendError> {
    let mut cum_dim = VecDeque::new();
    let mut cum = 1;
    for d in dims.iter().rev() {
        cum_dim.push_front(cum);
        cum *= *d;
    }
    let mut res = env.add_integer(0);
    for i in 0..lval.array_index.len() {
        let temp = lval.array_index.get(i).unwrap().generate_ir(env)?;
        let integer = env.add_integer(cum_dim[i]);
        let temp = env.add_binary_inst(ir::BinaryOp::Mul, temp, integer);
        res = env.add_binary_inst(ir::BinaryOp::Add, res, temp);
    }
    let ptr = env.add_getelemptr(var, res);
    Ok(ptr)
}
pub fn get_array_ptr_from_ptr(env: &mut Environment, var: Value, lval: &LVal, dims: &Rc<Vec<i32>>) -> Result<Value, FrontendError> {
    let mut cum_dim = VecDeque::new();
    cum_dim.push_front(1);
    let mut cum = 1;
    for d in dims.iter().rev() {
        cum *= *d;
        cum_dim.push_front(cum);
    }
    let mut res = env.add_integer(0);
    for i in 0..lval.array_index.len() {
        let temp = lval.array_index.get(i).unwrap().generate_ir(env)?;
        let integer = env.add_integer(cum_dim[i]);
        let temp = env.add_binary_inst(ir::BinaryOp::Mul, temp, integer);
        res = env.add_binary_inst(ir::BinaryOp::Add, res, temp);
    }
    let ptr = env.add_get_ptr(var, res);
    Ok(ptr)
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
                    Some(SymbolEntry::FuncParam(var)) => {
                        Ok(var)
                    },
                    Some(SymbolEntry::Const(value)) => {
                        let const_value = env.add_integer(value);
                        Ok(const_value)
                    },
                    Some(SymbolEntry::Array(var, dims, _, _)) => {
                        if lval.array_index.len() != dims.len() {
                            get_array_ptr(env, var, lval, &dims)
                        } else {
                            let ptr = get_array_pos(env, var, lval, &dims, lval.ident.clone())?;
                            let res = env.add_load(ptr);
                            Ok(res)
                        }
                    },
                    Some(SymbolEntry::ArrayPtr(var, dims)) => {
                        if lval.array_index.len() != dims.len() + 1{
                            get_array_ptr_from_ptr(env, var, lval, &dims)
                        } else {
                            let ptr = get_array_pos_from_ptr(env, var, lval, &dims)?;
                            let res = env.add_load(ptr);
                            Ok(res)
                        }
                    }
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