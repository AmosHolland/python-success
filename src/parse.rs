use rustpython_parser::{
    ast::{self, Stmt},
    Parse,
};

pub fn _parse_python(code: &str) -> Result<Vec<Stmt>, ()> {
    let ast_result = ast::Suite::parse(code, "<embedded>");

    if let Ok(ast) = ast_result {
        return Ok(ast);
    }

    Err(())
}
