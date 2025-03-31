use rustpython_parser::{
    ast::{self, Stmt},
    Parse,
};

pub fn parse_python(code: &str) -> Result<Vec<Stmt>, String> {
    let ast_result = ast::Suite::parse(code, "<embedded>");

    match ast_result {
        Ok(ast) => Ok(ast),
        Err(err) => Err(format!("{0}", err.error)),
    }
}
