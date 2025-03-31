use std::{fs::File, io::Read};

use ast_processing::translate_python;
use constraints::constraints_to_refute;
use parse::parse_python;
use pcf::parse_pcf_string;
use rustpython_parser::ast::Stmt;
use solver::solve_constraints;
use types::parse_type_string;

mod ast_processing;
mod constraints;
mod parse;
mod pcf;
mod solver;
mod types;

fn main() {
    // let new_t = parse_type_string("Ok");
    // let Ok(t) = new_t else {
    //     panic!("aaa");
    // };

    // println!("parsed type: {t}");

    match parse_program("examples/attribute_example.py".to_string()) {
        Ok(program) => {
            let translation_result = translate_python(program);
            match translation_result {
                Ok(term) => {
                    println!("{term:?}");
                    let constraints = constraints_to_refute(term.clone(), types::Type::Ok);
                    match solve_constraints(constraints) {
                        Ok(solved) => println!("Successfully refuted type Ok for term {term}, final constraints:\n{solved}"),
                        Err(err) => println!("Failed to refute type Ok for term {term}: {err}"),
                    }
                }
                Err(err) => println!("{err}"),
            }
        }
        Err(err) => println!("Encountered error while parsing program: {err}"),
    }
}

fn parse_program(path: String) -> Result<Vec<Stmt>, String> {
    let file_result = File::open(path);
    let Ok(mut file) = file_result else {
        return Err("Could not find file.".to_string());
    };

    let mut code = String::new();
    let Ok(_) = file.read_to_string(&mut code) else {
        return Err("Error reading file.".to_string());
    };

    parse_python(&code)
}

// TODO
// - finish parsing for terms and test basic constraint generation DONE
// - figure out constraints for new and implement constraint generation
//   for records, then test this DONE (maybe)
// - then inference if you get that far
//   - https://dl.acm.org/doi/pdf/10.1145/  165180.165188 <- this may be handy for a simple algorithm - hope it uses rewrite rules tbh
//   - WE ARE SO IN, rough algo will follow

// Repeat until all constraints are of the form Var ⊑ Type, or Type ⊑ Var or Var ⊑ Var
// - For all Type ⊑ Type constraints, transform if possible
// - For all VarA ⊑ Type1, and  Type2 ⊑ VarA , add Type2 ⊑ Type1
