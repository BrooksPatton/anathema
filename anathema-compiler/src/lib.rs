pub mod error;

pub(crate) mod compiler;
pub(crate) mod lexer;
pub(crate) mod parsing;
pub(crate) mod token;

use anathema_values::{Constants, ViewIds};
pub use compiler::Instruction;

use self::token::Tokens;

/// Compile source into instructions and constants.
pub fn compile(src: &str, view_ids: &mut ViewIds) -> error::Result<(Vec<Instruction>, Constants)> {
    let mut constants = Constants::new();
    let lexer = lexer::Lexer::new(src, &mut constants);
    let tokens = Tokens::new(lexer.collect::<error::Result<_>>()?, src.len());
    let parser = parsing::parser::Parser::new(tokens, &mut constants, src, view_ids);
    let expressions = parser.collect::<error::Result<Vec<_>>>()?;
    let optimizer = compiler::Optimizer::new(expressions);
    let expressions = optimizer.optimize();
    let compiler = compiler::Compiler::new(expressions);
    Ok((compiler.compile()?, constants))
}
