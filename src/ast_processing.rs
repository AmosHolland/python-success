use std::ffi::CStr;

use rustpython_parser::ast::{
    self, bigint::BigInt, CmpOp, Constant, Expr, ExprAttribute, ExprBinOp, ExprCall, ExprCompare,
    ExprConstant, ExprIfExp, ExprLambda, Operator, Stmt,
};

use num_traits::identities::Zero;

use crate::pcf::{PCFConst, PCFTerm};

pub fn translate_python(program: Vec<Stmt>) -> Result<PCFTerm, String> {
    let mut curr_term = None;
    for stmt in program.iter().rev() {
        let trans_result = translate_statement(stmt.clone(), curr_term);

        let Ok(new_term) = trans_result else {
            return trans_result;
        };

        curr_term = Some(new_term);
    }

    if let Some(term) = curr_term {
        Ok(term)
    } else {
        Err("Program was empty".to_string())
    }
}

fn translate_statement(stmt: Stmt, next_term: Option<PCFTerm>) -> Result<PCFTerm, String> {
    match next_term {
        None => {
            let Stmt::Expr(expr_statement) = stmt else {
                return Err("Final statement should be an expression.".to_string());
            };

            let expr_result = translate_expression(*expr_statement.value, None);
            let Ok(term) = expr_result else {
                return expr_result;
            };

            Ok(term)
        }
        Some(p_term) => {
            let Stmt::Assign(assign_stmt) = stmt else {
                return Err("Non final statements should be assignments.".to_string());
            };

            let target_result = translate_assign_target(assign_stmt.targets);
            let Ok((variable, label_opt)) = target_result else {
                return Err(target_result.expect_err(""));
            };

            let n_result = translate_expression(*assign_stmt.value, Some(&variable));
            let Ok(n) = n_result else { return n_result };

            if let Some(label) = label_opt {
                let let_target = format!("{variable}#1");
                let p = increase_n_object_assignments(p_term, &variable);
                let assign = PCFTerm::Assign(Box::new(PCFTerm::Var(variable)), label, Box::new(n));

                Ok(PCFTerm::Let(let_target, Box::new(assign), Box::new(p)))
            } else {
                Ok(PCFTerm::Let(variable, Box::new(n), Box::new(p_term)))
            }
        }
    }
}

fn translate_expression(expr: Expr, assign_target: Option<&String>) -> Result<PCFTerm, String> {
    match expr {
        Expr::BinOp(binop) => translate_binop(binop),
        Expr::Lambda(lambda) => translate_lambda(lambda, assign_target),
        Expr::IfExp(if_else) => translate_if(if_else),
        Expr::Call(call) => translate_call(call),
        Expr::Name(name) => Ok(PCFTerm::Var(name.id.to_string())),
        Expr::Constant(constant) => translate_constant(constant),
        Expr::Compare(comparison) => translate_compare(comparison),
        Expr::Attribute(attr_expr) => translate_attribute(attr_expr),
        _ => Err("Invalid Expression".to_string()),
    }
}

fn translate_assign_target(targets: Vec<Expr>) -> Result<(String, Option<String>), String> {
    if targets.len() != 1 {
        return Err("Assignments may only have one target".to_string());
    }

    let target = targets[0].clone();

    match target {
        Expr::Name(name) => Ok((name.id.to_string(), None)),
        Expr::Attribute(expr_attr) => {
            let Expr::Name(name) = *expr_attr.value else {
                return Err("Attribute assignment must be done on a variable.".to_string());
            };

            let label = expr_attr.attr.to_string();
            let var_name = name.id.to_string();

            Ok((var_name, Some(label)))
        }
        _ => Err("Assign target must be a variable or attribute.".to_string()),
    }
}

fn increase_n_object_assignments(term: PCFTerm, var_name: &String) -> PCFTerm {
    match term {
        PCFTerm::New => PCFTerm::New,
        PCFTerm::Var(x) => PCFTerm::Var(increment_var(x, var_name)),
        PCFTerm::Abs(x, m) => {
            PCFTerm::Abs(x, Box::new(increase_n_object_assignments(*m, var_name)))
        }
        PCFTerm::App(m, n) => PCFTerm::App(
            Box::new(increase_n_object_assignments(*m, var_name)),
            Box::new(increase_n_object_assignments(*n, var_name)),
        ),
        PCFTerm::If(m, n, p) => PCFTerm::If(
            Box::new(increase_n_object_assignments(*m, var_name)),
            Box::new(increase_n_object_assignments(*n, var_name)),
            Box::new(increase_n_object_assignments(*p, var_name)),
        ),
        PCFTerm::Let(x, n, p) => PCFTerm::Let(
            increment_var(x, var_name),
            Box::new(increase_n_object_assignments(*n, var_name)),
            Box::new(increase_n_object_assignments(*p, var_name)),
        ),
        PCFTerm::Assign(m, x, n) => PCFTerm::Assign(
            Box::new(increase_n_object_assignments(*m, var_name)),
            x,
            Box::new(increase_n_object_assignments(*n, var_name)),
        ),
        PCFTerm::Proj(m, x) => {
            PCFTerm::Proj(Box::new(increase_n_object_assignments(*m, var_name)), x)
        }
        PCFTerm::Fix(x, m) => {
            PCFTerm::Fix(x, Box::new(increase_n_object_assignments(*m, var_name)))
        }
        PCFTerm::Const(pcfconst) => PCFTerm::Const(pcfconst),
    }
}

fn increment_var(x: String, var_name: &String) -> String {
    let parts: Vec<&str> = x.split("#").collect();

    if !parts[0].eq(var_name) {
        return x;
    }

    if parts.len() == 1 {
        format!("{0}#1", parts[0])
    } else {
        format!(
            "{0}#{1}",
            parts[0],
            parts[1].parse::<i32>().expect("invalid variable extension") + 1
        )
    }
}

fn translate_binop(binop: ExprBinOp) -> Result<PCFTerm, String> {
    let left_result = translate_expression(*binop.left, None);
    let Ok(left) = left_result else {
        return left_result;
    };

    let right_result = translate_expression(*binop.right, None);
    let Ok(right) = right_result else {
        return right_result;
    };

    let op_result = match binop.op {
        Operator::Add => Ok(PCFTerm::Const(PCFConst::Plus)),
        Operator::Sub => Ok(PCFTerm::Const(PCFConst::Minus)),
        _ => Err("Binary operations must be + or -.".to_string()),
    };

    let Ok(op) = op_result else { return op_result };

    Ok(PCFTerm::App(
        Box::new(PCFTerm::App(Box::new(op), Box::new(left))),
        Box::new(right),
    ))
}

fn translate_if(if_expr: ExprIfExp) -> Result<PCFTerm, String> {
    let m_result = translate_expression(*if_expr.test, None);
    let Ok(m) = m_result else {
        return m_result;
    };

    let n_result = translate_expression(*if_expr.body, None);
    let Ok(n) = n_result else {
        return n_result;
    };

    let p_result = translate_expression(*if_expr.orelse, None);
    let Ok(p) = p_result else {
        return p_result;
    };

    Ok(PCFTerm::If(Box::new(m), Box::new(n), Box::new(p)))
}

fn translate_call(call: ExprCall) -> Result<PCFTerm, String> {
    if call.args.len() != 1 {
        return translate_empty_type(call);
    };

    let m_result = translate_expression(*call.func, None);
    let Ok(m) = m_result else {
        return m_result;
    };

    let n_result = translate_expression(call.args[0].clone(), None);
    let Ok(n) = n_result else {
        return n_result;
    };

    Ok(PCFTerm::App(Box::new(m), Box::new(n)))
}

fn translate_empty_type(call: ExprCall) -> Result<PCFTerm, String> {
    if !call.args.len() == 0 {
        return Err("Can only make a function call with one argument".to_string());
    };

    match *call.func {
        Expr::Call(expr_call) => {
            let Expr::Name(name) = *expr_call.func else {
                return Err("Invalid empty type form.".to_string());
            };
            if !name.id.to_string().eq("type") {
                return Err("Invalid empty type form.".to_string());
            }

            if expr_call.args.len() != 3 {
                return Err("Invalid empty type form.".to_string());
            }

            let Expr::Constant(expr_constant) = expr_call.args[0].clone() else {
                return Err("Invalid empty type form.".to_string());
            };

            let Constant::Str(str) = expr_constant.value else {
                return Err("Invalid empty type form.".to_string());
            };

            if !str.is_empty() {
                return Err("Invalid empty type form.".to_string());
            }

            let Expr::Tuple(expr_tuple) = expr_call.args[1].clone() else {
                return Err("Invalid empty type form.".to_string());
            };

            if !expr_tuple.elts.is_empty() {
                return Err("Invalid empty type form.".to_string());
            }

            let Expr::Dict(expr_dict) = expr_call.args[2].clone() else {
                return Err("Invalid empty type form.".to_string());
            };

            if !(expr_dict.keys.is_empty() && expr_dict.values.is_empty()) {
                return Err("Invalid empty type form.".to_string());
            }

            Ok(PCFTerm::New)
        }
        _ => Err("Invalid empty type form.".to_string()),
    }
}

fn translate_constant(constant: ExprConstant) -> Result<PCFTerm, String> {
    match constant.value {
        Constant::Int(big_int) => {
            let Ok(int) = big_int.to_string().parse() else {
                return Err("Invalid integer.".to_string());
            };

            Ok(PCFTerm::Const(PCFConst::Int(int)))
        }
        Constant::Bool(b) => {
            if b {
                Ok(PCFTerm::Const(PCFConst::True))
            } else {
                Ok(PCFTerm::Const(PCFConst::False))
            }
        }
        _ => Err("Constants must be integers, or booleans.".to_string()),
    }
}

fn translate_compare(comparison: ExprCompare) -> Result<PCFTerm, String> {
    if comparison.comparators.len() != 1 {
        return Err("Comparisons must me binary.".to_string());
    }
    if comparison.ops.len() != 1 {
        return Err("Comparisons must me binary.".to_string());
    }

    let left_result = translate_expression(*comparison.left, None);
    let Ok(left) = left_result else {
        return left_result;
    };

    let right_result = translate_expression(comparison.comparators[0].clone(), None);
    let Ok(right) = right_result else {
        return right_result;
    };

    let op_opt = match comparison.ops[0] {
        CmpOp::Eq => Some(PCFTerm::Const(PCFConst::Equal)),
        CmpOp::Lt => Some(PCFTerm::Const(PCFConst::LessThan)),
        _ => None,
    };

    let Some(op) = op_opt else {
        return Err("Comparison shold be == or <.".to_string());
    };

    Ok(PCFTerm::App(
        Box::new(PCFTerm::App(Box::new(op), Box::new(left))),
        Box::new(right),
    ))
}

fn translate_attribute(attr: ExprAttribute) -> Result<PCFTerm, String> {
    let label = attr.attr.to_string();

    let m_result = translate_expression(*attr.value, None);
    let Ok(m) = m_result else {
        return m_result;
    };

    Ok(PCFTerm::Proj(Box::new(m), label))
}

fn translate_lambda(lambda: ExprLambda, assign_target: Option<&String>) -> Result<PCFTerm, String> {
    if lambda.args.args.len() != 1 {
        return Err("Lambdas must only take one arg".to_string());
    }

    let x = lambda.args.args[0].def.arg.to_string();

    let m_result = translate_expression(*lambda.body, None);
    let Ok(m) = m_result else {
        return m_result;
    };

    if let Some(target) = assign_target {
        if term_contains_variable(&m, target) {
            let recurse_name = format!("r#{target}");
            let abs_m = term_rename_variable(m, target, &recurse_name);
            let inner_abs = PCFTerm::Abs(x, Box::new(abs_m));
            Ok(PCFTerm::Fix(recurse_name, Box::new(inner_abs)))
        } else {
            Ok(PCFTerm::Abs(x, Box::new(m)))
        }
    } else {
        Ok(PCFTerm::Abs(x, Box::new(m)))
    }
}

fn term_contains_variable(term: &PCFTerm, var_name: &String) -> bool {
    match term {
        PCFTerm::New => false,
        PCFTerm::Var(name) => name.eq(var_name),
        PCFTerm::Abs(_, m) => term_contains_variable(&m, var_name),
        PCFTerm::App(m, n) => {
            term_contains_variable(&m, var_name) || term_contains_variable(&n, var_name)
        }
        PCFTerm::If(m, n, p) => {
            term_contains_variable(&m, var_name)
                || term_contains_variable(&n, var_name)
                || term_contains_variable(&p, var_name)
        }
        PCFTerm::Let(_, n, p) => {
            term_contains_variable(&n, var_name) || term_contains_variable(&p, var_name)
        }
        PCFTerm::Assign(m, _, n) => {
            term_contains_variable(&m, var_name) || term_contains_variable(&n, var_name)
        }
        PCFTerm::Proj(m, _) => term_contains_variable(&m, var_name),
        PCFTerm::Fix(_, m) => term_contains_variable(&m, var_name),
        PCFTerm::Const(_) => false,
    }
}

fn term_rename_variable(term: PCFTerm, from: &String, to: &String) -> PCFTerm {
    match term {
        PCFTerm::New => PCFTerm::New,
        PCFTerm::Var(name) => {
            if name.eq(from) {
                PCFTerm::Var(to.to_string())
            } else {
                PCFTerm::Var(name)
            }
        }
        PCFTerm::Abs(x, m) => PCFTerm::Abs(x, Box::new(term_rename_variable(*m, from, to))),
        PCFTerm::App(m, n) => PCFTerm::App(
            Box::new(term_rename_variable(*m, from, to)),
            Box::new(term_rename_variable(*n, from, to)),
        ),
        PCFTerm::If(m, n, p) => PCFTerm::If(
            Box::new(term_rename_variable(*m, from, to)),
            Box::new(term_rename_variable(*n, from, to)),
            Box::new(term_rename_variable(*p, from, to)),
        ),
        PCFTerm::Let(x, n, p) => PCFTerm::Let(
            x,
            Box::new(term_rename_variable(*n, from, to)),
            Box::new(term_rename_variable(*p, from, to)),
        ),
        PCFTerm::Assign(m, x, n) => PCFTerm::Assign(
            Box::new(term_rename_variable(*m, from, to)),
            x,
            Box::new(term_rename_variable(*n, from, to)),
        ),
        PCFTerm::Proj(m, x) => PCFTerm::Proj(Box::new(term_rename_variable(*m, from, to)), x),
        PCFTerm::Fix(x, m) => PCFTerm::Fix(x, Box::new(term_rename_variable(*m, from, to))),
        PCFTerm::Const(c) => PCFTerm::Const(c),
    }
}
