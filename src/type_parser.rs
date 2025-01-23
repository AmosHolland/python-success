use std::collections::HashMap;

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use crate::types::{Type, TypeVariable};

#[derive(Parser)]
#[grammar = "types.pest"]
struct TypeParser;

pub fn parse_type_string(type_string: &str) -> Result<Type, &str> {
    let type_info = TypeParser::parse(Rule::r#type, type_string);

    match type_info {
        Ok(mut pairs) => {
            let top_level = pairs
                .next()
                .expect("Encountered an empty type while parsing type.");

            let mut ast_traverser = TypeASTTraverser {
                n_vars: 0,
                var_map: HashMap::new(),
            };

            Ok(ast_traverser.parse_type(top_level))
        }

        Err(_) => Err("Failed to parse type string."),
    }
}

struct TypeASTTraverser {
    n_vars: usize,
    var_map: HashMap<String, usize>,
}

impl TypeASTTraverser {
    fn parse_type(&mut self, pair: Pair<Rule>) -> Type {
        match pair.as_rule() {
            Rule::num_type => Type::Num,
            Rule::var_type => {
                let name = pair
                    .into_inner()
                    .next()
                    .expect("Encountered empty var while parsing type.")
                    .as_str()
                    .to_string();

                let id = if self.var_map.contains_key(&name) {
                    self.var_map[&name]
                } else {
                    self.n_vars += 1;
                    self.var_map.insert(name.clone(), self.n_vars - 1);
                    self.n_vars - 1
                };

                Type::Var(TypeVariable { id, name })
            }
            Rule::r#type => self.parse_type(
                pair.into_inner()
                    .next()
                    .expect("Encountered empty type while parsing type."),
            ),
            Rule::arrow_type => {
                let mut inner_types = Vec::new();

                for p in pair.into_inner() {
                    inner_types.push(self.parse_type(p));
                }
                Type::Arrow(
                    Box::new(inner_types[0].clone()),
                    Box::new(inner_types[1].clone()),
                )
            }
            _ => unreachable!(),
        }
    }
}
