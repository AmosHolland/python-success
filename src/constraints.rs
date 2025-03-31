use std::collections::HashMap;

use crate::{
    pcf::PCFTerm,
    types::{Row, Type},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubtypeConstraint {
    pub left: Type,
    pub right: Type,
}

impl std::fmt::Display for SubtypeConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{0} ⊑ {1}", self.left, self.right)
    }
}

impl SubtypeConstraint {
    pub fn new(left: Type, right: Type) -> Self {
        SubtypeConstraint { left, right }
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintSet {
    pub constraints: Vec<SubtypeConstraint>,
}

impl std::fmt::Display for ConstraintSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_str = "".to_string();
        for constraint in self.constraints.iter() {
            fmt_str = format!("{fmt_str}{constraint}\n");
        }

        write!(f, "{fmt_str}")
    }
}

impl ConstraintSet {
    fn new(constraints: Vec<SubtypeConstraint>) -> Self {
        ConstraintSet { constraints }
    }

    fn union(&self, other: ConstraintSet) -> ConstraintSet {
        let mut constraints = self.constraints.clone();
        constraints.append(&mut other.constraints.clone());

        ConstraintSet::new(constraints)
    }

    pub fn are_atomic(&self) -> bool {
        for constraint in self.constraints.iter() {
            match (&constraint.left, &constraint.right) {
                (Type::Var(_), _) => (),
                (_, Type::Var(_)) => (),
                (Type::Row(Row::Var(_)), _) => (),
                (_, Type::Row(Row::Var(_))) => (),
                (Type::Row(Row::Without((row, _))), _) => {
                    if let Row::Var(_) = **row {
                    } else {
                        return false;
                    }
                }
                (_, Type::Row(Row::Without((row, _)))) => {
                    if let Row::Var(_) = **row {
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
        }

        true
    }
}

struct TypeVariablePool {
    var_chars: Vec<char>,
    prefix: String,
    row_var: bool,
}

type TypeContext = HashMap<String, Type>;

impl TypeVariablePool {
    fn new(prefix: String, row_var: bool) -> Self {
        TypeVariablePool {
            var_chars: vec!['A'],
            prefix,
            row_var,
        }
    }

    fn get_fresh_name(&mut self) -> String {
        let new_var = self.var_chars.iter().collect::<String>();

        let mut updated = false;
        let n_chars = self.var_chars.len();

        for i in 0..n_chars {
            let index = n_chars - (i + 1);
            if self.var_chars[index] != 'Z' {
                self.var_chars[index] = ((self.var_chars[index] as u8) + 1) as char;
                updated = true;

                for j in index + 1..n_chars {
                    self.var_chars[j] = 'A';
                }
                break;
            }
        }

        if !updated {
            self.var_chars = vec!['A'; n_chars + 1];
        }

        format!("{0}{new_var}", self.prefix)
    }

    fn get_fresh_var(&mut self) -> Type {
        let new_name = self.get_fresh_name();

        if !self.row_var {
            Type::Var(new_name)
        } else {
            Type::Row(Row::Var(new_name))
        }
    }

    fn get_fresh_row(&mut self) -> Row {
        let new_name = self.get_fresh_name();

        Row::Var(new_name)
    }
}

pub fn constraints_to_refute(term: PCFTerm, to_refute: Type) -> ConstraintSet {
    let context = HashMap::new();
    let mut type_pool = TypeVariablePool::new("".to_string(), false);
    let mut row_pool = TypeVariablePool::new("R_".to_string(), true);

    let (t1, c1) = generate_constraints(context, term, &mut type_pool, &mut row_pool);
    c1.union(ConstraintSet::new(vec![
        SubtypeConstraint::new(to_refute.clone(), t1.clone()),
        SubtypeConstraint::new(t1, to_refute),
    ]))
}

fn generate_constraints(
    context: TypeContext,
    term: PCFTerm,
    type_var_pool: &mut TypeVariablePool,
    row_var_pool: &mut TypeVariablePool,
) -> (Type, ConstraintSet) {
    match term {
        PCFTerm::Var(var_name) => {
            let x = type_var_pool.get_fresh_var();
            let Some(t) = context.get(&var_name) else {
                panic!("Malformed term at the function.")
            };
            let c = ConstraintSet::new(vec![SubtypeConstraint::new(x.clone(), t.clone())]);

            (x, c)
        }
        PCFTerm::Const(constant) => {
            let x = type_var_pool.get_fresh_var();
            let t = constant.get_type();
            let c = ConstraintSet::new(vec![SubtypeConstraint::new(x.clone(), t)]);

            (x, c)
        }
        PCFTerm::Abs(var_name, n) => {
            let x = type_var_pool.get_fresh_var();
            let t2 = type_var_pool.get_fresh_var();
            let t2_prime = type_var_pool.get_fresh_var();

            let mut context_prime = context.clone();
            context_prime.insert(var_name, t2.clone());

            let (t1, c1) = generate_constraints(context_prime, *n, type_var_pool, row_var_pool);

            let c = c1.union(ConstraintSet::new(vec![
                SubtypeConstraint::new(t2, t2_prime.clone()),
                SubtypeConstraint::new(
                    x.clone(),
                    Type::Coto(Box::new(t2_prime.clone()), Box::new(t1.clone())),
                ),
                SubtypeConstraint::new(Type::Coto(Box::new(t2_prime), Box::new(t1)), x.clone()),
            ]));

            (x, c)
        }
        PCFTerm::App(m, n) => {
            let x = type_var_pool.get_fresh_var();
            let t2_prime = type_var_pool.get_fresh_var();
            let (t1, c1) = generate_constraints(context.clone(), *m, type_var_pool, row_var_pool);
            let (t2, c2) = generate_constraints(context.clone(), *n, type_var_pool, row_var_pool);

            let c = c1.union(c2).union(ConstraintSet::new(vec![
                SubtypeConstraint::new(t2_prime.clone(), t2),
                SubtypeConstraint::new(
                    t1.clone(),
                    Type::Coto(Box::new(t2_prime.clone()), Box::new(x.clone())),
                ),
                SubtypeConstraint::new(
                    Type::Coto(Box::new(t2_prime), Box::new(x.clone())),
                    t1.clone(),
                ),
            ]));

            (x, c)
        }
        PCFTerm::If(_, n, p) => {
            let x = type_var_pool.get_fresh_var();
            let (t1, c1) = generate_constraints(context.clone(), *n, type_var_pool, row_var_pool);
            let (t2, c2) = generate_constraints(context.clone(), *p, type_var_pool, row_var_pool);

            let c = c1.union(c2).union(ConstraintSet::new(vec![
                SubtypeConstraint::new(x.clone(), t1),
                SubtypeConstraint::new(x.clone(), t2),
            ]));

            (x, c)
        }
        PCFTerm::Fix(var_name, m) => {
            let x = type_var_pool.get_fresh_var();
            let t2 = type_var_pool.get_fresh_var();

            let mut context_prime = context.clone();
            context_prime.insert(var_name, t2.clone());

            let (t1, c1) = generate_constraints(context_prime, *m, type_var_pool, row_var_pool);

            let c = c1.union(ConstraintSet::new(vec![
                SubtypeConstraint::new(x.clone(), t2.clone()),
                SubtypeConstraint::new(t1.clone(), t2.clone()),
                SubtypeConstraint::new(t2, t1),
            ]));

            (x, c)
        }
        PCFTerm::Let(var_name, n, p) => {
            let x = type_var_pool.get_fresh_var();
            let t3 = type_var_pool.get_fresh_var();

            let mut context_prime = context.clone();
            context_prime.insert(var_name, t3.clone());

            let (t1, c1) = generate_constraints(context, *n, type_var_pool, row_var_pool);
            let (t2, c2) = generate_constraints(context_prime, *p, type_var_pool, row_var_pool);

            let c = c1.union(c2).union(ConstraintSet::new(vec![
                SubtypeConstraint::new(x.clone(), t2.clone()),
                SubtypeConstraint::new(t2, x.clone()),
                SubtypeConstraint::new(t1.clone(), t3.clone()),
                SubtypeConstraint::new(t3, t1),
            ]));

            (x, c)
        }
        PCFTerm::New => {
            let v = row_var_pool.get_fresh_var();

            let c = ConstraintSet::new(vec![SubtypeConstraint::new(
                v.clone(),
                Type::Row(Row::NonEmpty),
            )]);

            (v, c)
        }
        PCFTerm::Assign(m, label, _) => {
            let v = row_var_pool.get_fresh_row();

            let (r1, c1) = generate_constraints(context, *m, type_var_pool, row_var_pool);

            let c = c1.union(ConstraintSet::new(vec![
                SubtypeConstraint::new(
                    r1.clone(),
                    Type::Row(Row::Without((Box::new(v.clone()), label.clone()))),
                ),
                SubtypeConstraint::new(Type::Row(Row::Without((Box::new(v.clone()), label))), r1),
            ]));

            (Type::Row(v), c)
        }
        PCFTerm::Proj(m, label) => {
            let x = type_var_pool.get_fresh_var();

            let (r1, c1) = generate_constraints(context, *m, type_var_pool, row_var_pool);

            let c = c1.union(ConstraintSet::new(vec![
                SubtypeConstraint::new(
                    r1.clone(),
                    Type::Row(Row::from_label_type(label.clone(), x.clone())),
                ),
                SubtypeConstraint::new(Type::Row(Row::from_label_type(label, x.clone())), r1),
            ]));

            (x, c)
        }
    }
}
