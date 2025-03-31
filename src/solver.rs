use std::{any::TypeId, clone, collections::HashMap, env::var, ops::Sub, sync::atomic::AtomicBool};

use crate::{
    constraints::{ConstraintSet, SubtypeConstraint},
    types::{Row, Type},
};

pub fn solve_constraints(constraint_set: ConstraintSet) -> Result<ConstraintSet, String> {
    let mut prev_changed = true;
    let mut prev_atomic = false;

    let mut explored_connections: HashMap<String, Vec<(Type, Type)>> = HashMap::new();

    let mut prev_constraints = constraint_set.clone();
    let mut count = 0;
    loop {
        let mut found_new_constraints;

        match resolve_non_atomics(prev_constraints) {
            Err(s) => return Err(s),
            Ok((next_constraints, has_new_constraints)) => {
                found_new_constraints = has_new_constraints;
                prev_constraints = ConstraintSet {
                    constraints: next_constraints,
                }
            }
        }

        let transitive_constraints =
            resolve_inductives(&prev_constraints, &mut explored_connections);

        let mut has_new_transitives = false;
        for new in transitive_constraints {
            if !prev_constraints.constraints.contains(&new) {
                prev_constraints.constraints.push(new);
                has_new_transitives = true;
            }
        }

        found_new_constraints = found_new_constraints || has_new_transitives;

        let not_changing = !found_new_constraints && !prev_changed;
        let is_atomic = prev_constraints.are_atomic();

        if (is_atomic && prev_atomic) && not_changing {
            return Ok(prev_constraints);
        } else if not_changing {
            return Err("Failed to resolve constraints.".to_string());
        }

        prev_changed = found_new_constraints;
        prev_atomic = is_atomic;

        count += 1;
    }
}

fn resolve_non_atomics(
    constraint_set: ConstraintSet,
) -> Result<(Vec<SubtypeConstraint>, bool), String> {
    let mut next_constraints = Vec::new();
    let mut next_has_new = false;
    for constraint in constraint_set.constraints.iter() {
        match transform_subtype(constraint) {
            Ok(cs) => {
                next_constraints.extend(cs.clone());
                if !(cs.is_empty() || (cs.len() == 1 && cs[0] == *constraint)) {
                    next_has_new = true;
                }
            }
            Err(msg) => return Err(msg),
        }
    }
    Ok((next_constraints, next_has_new))
}

fn resolve_inductives(
    constraint_set: &ConstraintSet,
    explored_connections: &mut HashMap<String, Vec<(Type, Type)>>,
) -> Vec<SubtypeConstraint> {
    let mut new_constraints = Vec::new();
    for constraint in constraint_set.constraints.iter() {
        if let Type::Var(var) = &constraint.left {
            new_constraints.extend(generate_transitives(
                constraint_set,
                explored_connections,
                var.clone(),
                constraint.right.clone(),
                None,
            ));
        }

        if let Type::Row(Row::Var(var)) = &constraint.left {
            new_constraints.extend(generate_transitives(
                constraint_set,
                explored_connections,
                var.clone(),
                constraint.right.clone(),
                None,
            ));
        }

        if let Type::Row(Row::Without((row, label))) = &constraint.left {
            if let Row::Var(var) = *row.clone() {
                new_constraints.extend(generate_transitives(
                    constraint_set,
                    explored_connections,
                    var,
                    constraint.right.clone(),
                    Some((label.to_string(), false)),
                ))
            }
        }

        if let Type::Row(Row::Without((row, label))) = &constraint.right {
            if let Row::Var(var) = *row.clone() {
                new_constraints.extend(generate_transitives(
                    constraint_set,
                    explored_connections,
                    var,
                    constraint.left.clone(),
                    Some((label.to_string(), true)),
                ))
            }
        }
    }

    new_constraints
}

fn generate_transitives(
    constraint_set: &ConstraintSet,
    explored_connections: &mut HashMap<String, Vec<(Type, Type)>>,
    var: String,
    trans_1: Type,
    without_info: Option<(String, bool)>,
) -> Vec<SubtypeConstraint> {
    let lookup_name = if without_info.is_some() {
        format!("W_{var}")
    } else {
        var.clone()
    };
    let mut explored_transitives = if let Some(transitives) = explored_connections.get(&lookup_name)
    {
        transitives.clone()
    } else {
        vec![]
    };

    let trans_2_candidates = collect_potential_transitives(&var, constraint_set);

    let mut new_constraints = Vec::new();

    // this could be better, split into paths to generate l and r, then do the pushes after.
    for trans_2 in trans_2_candidates {
        let l_r_opt = match without_info.clone() {
            None => {
                if !explored_transitives.contains(&(trans_2.clone(), trans_1.clone())) {
                    Some((trans_2, trans_1.clone()))
                } else {
                    None
                }
            }
            Some((label, var_is_r)) => {
                let trans_2_without = match trans_2 {
                    Type::Row(row) => Type::Row(row.without(label)),
                    Type::Var(var) => Type::Row(Row::Var(var).without(label)),
                    _ => panic!("invalid row subtype present at inductive stage."),
                };

                if var_is_r {
                    if !explored_transitives.contains(&(trans_1.clone(), trans_2_without.clone())) {
                        Some((trans_1.clone(), trans_2_without))
                    } else {
                        None
                    }
                } else if !explored_transitives
                    .contains(&(trans_2_without.clone(), trans_1.clone()))
                {
                    Some((trans_2_without, trans_1.clone()))
                } else {
                    None
                }
            }
        };

        if let Some((l, r)) = l_r_opt {
            explored_transitives.push((l.clone(), r.clone()));
            new_constraints.push(SubtypeConstraint::new(l, r));
        }
    }

    explored_connections.insert(lookup_name, explored_transitives);
    new_constraints
}

fn collect_potential_transitives(var_name: &String, constraint_set: &ConstraintSet) -> Vec<Type> {
    let mut candiates = Vec::new();
    for constraint in constraint_set.constraints.iter() {
        if let Type::Var(name) = &constraint.right {
            if name.eq(var_name) {
                candiates.push(constraint.left.clone());
            }
        } else if let Type::Row(Row::Var(name)) = &constraint.right {
            if name.eq(var_name) {
                candiates.push(constraint.left.clone());
            }
        }
    }
    candiates
}

fn transform_subtype(constraint: &SubtypeConstraint) -> Result<Vec<SubtypeConstraint>, String> {
    match (constraint.left.clone(), constraint.right.clone()) {
        (Type::Var(_), _) => Ok(vec![constraint.clone()]),
        (_, Type::Var(_)) => Ok(vec![constraint.clone()]),

        (Type::Row(rowl), Type::Row(rowr)) => transform_row_subtype(rowl, rowr),
        (Type::Row(_), _) => Err("Row subtyped to non-row".to_string()),
        (_, Type::Row(_)) => Err("Non-row subtyped to row.".to_string()),

        (Type::Coto(t1, t2), Type::Coto(t3, t4)) => Ok(vec![
            SubtypeConstraint::new(*t3, *t1),
            SubtypeConstraint::new(*t2, *t4),
        ]),
        (Type::Coto(..), _) => Err("Coto subtyped to top-level type.".to_string()),
        (_, Type::Coto(..)) => Err("Top-level type subtyped to coto.".to_string()),

        (Type::Int, Type::Int) => Ok(vec![]),
        (_, Type::Int) => Err("Invalid subtype to Int.".to_string()),

        (Type::CompBool, Type::CompBool) => Ok(vec![]),
        (Type::Int, Type::CompBool) => Ok(vec![]),
        (_, Type::CompBool) => Err("Invalid subtype to Comp(Bool).".to_string()),

        (Type::Bool, Type::Bool) => Ok(vec![]),
        (_, Type::Bool) => Err("Invalid subtype to Bool.".to_string()),

        (Type::CompInt, Type::CompInt) => Ok(vec![]),
        (Type::Bool, Type::CompInt) => Ok(vec![]),
        (_, Type::CompInt) => Err("Invalid subtype to Comp(Int).".to_string()),

        (Type::Ok, Type::Ok) => Ok(vec![]),
        (Type::Int, Type::Ok) => Ok(vec![]),
        (Type::Bool, Type::Ok) => Ok(vec![]),
        (_, Type::Ok) => Err("Invalid subtype to Ok".to_string()),
    }
}

fn transform_row_subtype(left: Row, right: Row) -> Result<Vec<SubtypeConstraint>, String> {
    let constraint = Ok(vec![SubtypeConstraint::new(
        Type::Row(left.clone()),
        Type::Row(right.clone()),
    )]);

    match (left, right) {
        (Row::Var(_), _) => constraint,
        (_, Row::Var(_)) => constraint,

        (Row::Without((row, label)), r) => Ok(vec![SubtypeConstraint::new(
            Type::Row(row.without(label)),
            Type::Row(r),
        )]),
        (l, Row::Without((row, label))) => Ok(vec![SubtypeConstraint::new(
            Type::Row(l),
            Type::Row(row.without(label)),
        )]),

        (_, Row::Empty) => Ok(vec![]),
        (Row::Empty, _) => Err("Empty row subtyped to non-empty row.".to_string()),

        (Row::NonEmpty, Row::NonEmpty) => Ok(vec![]),
        (Row::Labels(..), Row::NonEmpty) => Ok(vec![]),
        (Row::NonEmpty, Row::Labels(..)) => {
            Err("Generic non-empty subtyped to specific non-empty.".to_string())
        }

        (Row::Labels(l_labels), Row::Labels(r_labels)) => {
            let mut constraints = Vec::new();
            let mut invalid = false;
            for (label, right_t) in r_labels {
                let Some(left_t) = l_labels.get(&label) else {
                    invalid = true;
                    break;
                };
                constraints.push(SubtypeConstraint::new(left_t.clone(), right_t));
            }

            if invalid {
                Err("Mismatching labels in subtyped rows.".to_string())
            } else {
                Ok(constraints)
            }
        }
    }
}
