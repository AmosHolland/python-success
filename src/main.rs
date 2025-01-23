use type_parser::parse_type_string;

mod parse;
mod type_parser;
mod types;

fn main() {
    let new_t = parse_type_string("((Num) -> (A)) -> (A)");
    if let Ok(t) = new_t {
        println!("parsed_type: {t}");
    }
}
