use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use crate::types::Type;

#[derive(Debug, Clone)]
pub enum PCFConst {
    True,
    False,
    Int(i32),
    Plus,
    Minus,
    LessThan,
    Equal,
}

impl std::fmt::Display for PCFConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PCFConst::True => write!(f, "true"),
            PCFConst::False => write!(f, "false"),
            PCFConst::Int(int) => write!(f, "{int}"),
            PCFConst::Plus => write!(f, "(+)"),
            PCFConst::Minus => write!(f, "(-)"),
            PCFConst::LessThan => write!(f, "(<)"),
            PCFConst::Equal => write!(f, "(=)>"),
        }
    }
}

impl PCFConst {
    pub fn get_type(&self) -> Type {
        match self {
            PCFConst::True => Type::CompBool,
            PCFConst::False => Type::CompBool,
            PCFConst::Int(_) => Type::CompInt,
            PCFConst::Plus => Type::Coto(
                Box::new(Type::Int),
                Box::new(Type::Coto(Box::new(Type::Int), Box::new(Type::Int))),
            ),
            PCFConst::Minus => Type::Coto(
                Box::new(Type::Int),
                Box::new(Type::Coto(Box::new(Type::Int), Box::new(Type::Int))),
            ),
            PCFConst::LessThan => Type::Coto(
                Box::new(Type::Int),
                Box::new(Type::Coto(Box::new(Type::Int), Box::new(Type::Bool))),
            ),
            PCFConst::Equal => Type::Coto(
                Box::new(Type::Int),
                Box::new(Type::Coto(Box::new(Type::Int), Box::new(Type::Bool))),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PCFTerm {
    New,
    Var(PCFVar),
    Abs(PCFVar, PCFRTerm),
    App(PCFRTerm, PCFRTerm),
    If(PCFRTerm, PCFRTerm, PCFRTerm),
    Let(PCFVar, PCFRTerm, PCFRTerm),
    Assign(PCFRTerm, PCFVar, PCFRTerm),
    Proj(PCFRTerm, PCFVar),
    Fix(PCFVar, PCFRTerm),
    Const(PCFConst),
}

impl std::fmt::Display for PCFTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PCFTerm::New => write!(f, "new()"),
            PCFTerm::Var(x) => write!(f, "{x}"),
            PCFTerm::Abs(x, m) => write!(f, "(λ{x}. {m})"),
            PCFTerm::App(m, n) => write!(f, "{m}({n})"),
            PCFTerm::If(m, n, p) => write!(f, "if ({m}) then ({n}) else ({p})"),
            PCFTerm::Let(x, n, p) => write!(f, "let {x} = ({n}) in ({p})"),
            PCFTerm::Assign(m, x, n) => write!(f, "{m}.{x} := {n}"),
            PCFTerm::Proj(m, x) => write!(f, "{m}.{x}"),
            PCFTerm::Fix(x, m) => write!(f, "fix {x}. {m}"),
            PCFTerm::Const(c) => write!(f, "{c}"),
        }
    }
}

pub type PCFRTerm = Box<PCFTerm>;
pub type PCFVar = String;

#[derive(Parser)]
#[grammar = "pcf.pest"]
struct PCFParser;

pub fn parse_pcf_string(pcf_string: &str) -> Result<PCFTerm, &str> {
    let pcf_info = PCFParser::parse(Rule::term, pcf_string);

    match pcf_info {
        Ok(mut pairs) => {
            let top_level = pairs
                .next()
                .expect("Encountered an empty term while parsing pcf.");

            Ok(parse_pcf(top_level))
        }

        Err(_) => Err("Failed to parse pcf string."),
    }
}

pub fn parse_pcf(pair: Pair<Rule>) -> PCFTerm {
    match pair.as_rule() {
        Rule::var => {
            let name = pair.as_str().to_string();

            PCFTerm::Var(name)
        }
        Rule::r#const => {
            let inner = pair.into_inner().next().expect("Const without inner.");

            PCFTerm::Const(parse_pcf_const(inner))
        }
        Rule::abs => {
            let mut inner_iter = pair.into_inner();

            let x = inner_iter.next().expect("Encountered ABS without x");
            let m = inner_iter.next().expect("Encountered ABS without M.");

            PCFTerm::Abs(x.as_str().to_string(), Box::new(parse_pcf(m)))
        }
        Rule::app => {
            let mut inner_iter = pair.into_inner();

            let m = inner_iter.next().expect("Encountered APP without M.");
            let n = inner_iter.next().expect("Encountered APP without N");

            PCFTerm::App(Box::new(parse_pcf(m)), Box::new(parse_pcf(n)))
        }
        Rule::r#if => {
            let mut inner_iter = pair.into_inner();

            let m = inner_iter.next().expect("Encountered IF without M");
            let n = inner_iter.next().expect("Encountered IF without N");
            let p = inner_iter.next().expect("Encountered IF without P");

            PCFTerm::If(
                Box::new(parse_pcf(m)),
                Box::new(parse_pcf(n)),
                Box::new(parse_pcf(p)),
            )
        }
        Rule::r#let => {
            let mut inner_iter = pair.into_inner();

            let x = inner_iter.next().expect("Encountered LET without x");
            let n = inner_iter.next().expect("Encountered LET without N");
            let p = inner_iter.next().expect("Encountered LET without P");

            PCFTerm::Let(
                x.as_str().to_string(),
                Box::new(parse_pcf(n)),
                Box::new(parse_pcf(p)),
            )
        }
        Rule::fix => {
            let mut inner_iter = pair.into_inner();

            let x = inner_iter.next().expect("Encountered FIX without x");
            let m = inner_iter.next().expect("Encountered FIX without M");

            PCFTerm::Fix(x.as_str().to_string(), Box::new(parse_pcf(m)))
        }
        Rule::new => PCFTerm::New,
        Rule::assign => {
            let mut inner_iter = pair.into_inner();

            let m = inner_iter.next().expect("Encountered ASSIGN without M");
            let x = inner_iter.next().expect("Encountered ASSIGN without x");
            let n = inner_iter.next().expect("Encountered ASSIGN without N");

            PCFTerm::Assign(
                Box::new(parse_pcf(m)),
                x.as_str().to_string(),
                Box::new(parse_pcf(n)),
            )
        }
        Rule::proj => {
            let mut inner_iter = pair.into_inner();

            let m = inner_iter.next().expect("Encountered PROJ without M");
            let x = inner_iter.next().expect("Encountered PROJ without x");

            PCFTerm::Proj(Box::new(parse_pcf(m)), x.as_str().to_string())
        }
        _ => parse_pcf(
            pair.into_inner()
                .next()
                .expect("Encountered Empty Term or Recursive Term"),
        ),
    }
}

pub fn parse_pcf_const(pair: Pair<Rule>) -> PCFConst {
    match pair.as_rule() {
        Rule::int => {
            let val = pair
                .as_str()
                .parse()
                .expect("Encountered non integer int while parsing term.");

            PCFConst::Int(val)
        }
        Rule::r#true => PCFConst::True,
        Rule::r#false => PCFConst::False,
        Rule::plus => PCFConst::Plus,
        Rule::minus => PCFConst::Minus,
        Rule::less_than => PCFConst::LessThan,
        Rule::equal => PCFConst::Equal,
        _ => panic!("Encountered invalid const while parsing term."),
    }
}
