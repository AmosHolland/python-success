use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Coto(Box<Type>, Box<Type>),
    Int,
    Bool,
    Row(Row),
    CompBool,
    CompInt,
    Var(String),
    Ok,
}

type Label = String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Row {
    Empty,
    NonEmpty,
    Labels(AttributeVector),
    Without((Box<Row>, Label)),
    Var(String),
}

impl Row {
    pub fn from_label_type(label: String, t: Type) -> Self {
        let mut row_vec = HashMap::new();
        row_vec.insert(label, t);
        Row::Labels(row_vec)
    }

    pub fn without(&self, label: Label) -> Self {
        match self {
            Row::Empty => Row::Empty,
            Row::Labels(attrs) => {
                let mut new_attrs = attrs.clone();
                new_attrs.remove(&label);

                if new_attrs.is_empty() {
                    Row::Empty
                } else {
                    Row::Labels(new_attrs)
                }
            }
            _ => Row::Without((Box::new(self.clone()), label)),
        }
    }
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Row::Empty => write!(f, "{{}}"),
            Row::NonEmpty => write!(f, "{{ _ }}"),
            Row::Labels(vec) => {
                let mut label_assigns = "".to_string();
                for (label, t) in vec.iter() {
                    label_assigns = format!("{label_assigns}, {label} : {t}");
                }
                write!(f, "{{{label_assigns}}}")
            }
            Row::Without((row, label)) => write!(f, "{row} \\ {label}"),
            Row::Var(var_name) => write!(f, "{var_name}"),
        }
    }
}

type AttributeVector = HashMap<Label, Type>;

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Coto(t1, t2) => write!(f, "({0}) >/- ({1})", *t1.to_owned(), *t2.to_owned()),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Row(row) => write!(f, "{row}"),
            Type::CompInt => write!(f, "Comp(Int)"),
            Type::CompBool => write!(f, "Comp(Bool)"),
            Type::Var(type_variable) => write!(f, "{0}", type_variable),
            Type::Ok => write!(f, "Ok"),
        }
    }
}

type Variable = String;

struct TypeSubstitution {
    var_map: HashMap<Variable, Type>,
}

impl TypeSubstitution {
    fn new() -> Self {
        TypeSubstitution {
            var_map: HashMap::new(),
        }
    }

    fn apply(&self, t: Type) -> Type {
        match t {
            Type::Coto(l, r) => Type::Coto(Box::new(self.apply(*l)), Box::new(self.apply(*r))),
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Var(var) => {
                if self.var_map.contains_key(&var) {
                    self.var_map[&var].clone()
                } else {
                    Type::Var(var)
                }
            }
            _ => todo!(),
        }
    }
}

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

            Ok(parse_type(top_level))
        }

        Err(_) => Err("Failed to parse type string."),
    }
}

fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::num_type => Type::Int,
        Rule::bool_type => Type::Bool,
        Rule::var_type => {
            let name = pair
                .into_inner()
                .next()
                .expect("Encountered empty var while parsing type.")
                .as_str()
                .to_string();

            Type::Var(name)
        }
        Rule::ok_type => Type::Ok,
        Rule::r#type => parse_type(
            pair.into_inner()
                .next()
                .expect("Encountered empty type while parsing type."),
        ),
        Rule::arrow_type => {
            let mut inner_types = Vec::new();

            for p in pair.into_inner() {
                inner_types.push(parse_type(p));
            }
            Type::Coto(
                Box::new(inner_types[0].clone()),
                Box::new(inner_types[1].clone()),
            )
        }
        _ => unreachable!(),
    }
}
