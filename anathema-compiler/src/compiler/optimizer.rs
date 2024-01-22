use anathema_values::{StringId, ValueId, ViewId, Visibility};

use crate::parsing::parser::Statement as ParseStatement;

enum ControlFlow {
    If(ValueId),
    Else(Option<ValueId>),
}

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub(crate) enum Statement {
    If {
        cond: ValueId,
        size: usize,
    },
    Else {
        cond: Option<ValueId>,
        size: usize,
    },
    For {
        data: ValueId,
        binding: StringId,
        size: usize,
    },
    View(ViewId),
    LoadText(ValueId),
    LoadAttribute {
        key: StringId,
        value: ValueId,
    },
    Node {
        ident: StringId,
        scope_size: usize,
    },
    Declaration {
        visibility: Visibility,
        binding: StringId,
        value: ValueId,
    },
    Assignment {
        binding: StringId,
        value: ValueId,
    },
}

pub(crate) struct Optimizer {
    output: Vec<Statement>,
    input: Vec<ParseStatement>,
    ep: usize,
}

impl Optimizer {
    pub(crate) fn new(input: Vec<ParseStatement>) -> Self {
        Self {
            output: vec![],
            input,
            ep: 0,
        }
    }

    // -----------------------------------------------------------------------------
    //     - Optimize -
    //
    //     * Collapse empty if to just the body of the else if it exists
    //     * Remove empty else
    //     * Remove empty if
    //     * Remove empty for-loops
    //
    //     Possible future optimizations
    //     * Attribute keys could be string slices
    //     * Node idents could also be looked up beforehand
    // -----------------------------------------------------------------------------

    pub(crate) fn optimize(mut self) -> Vec<Statement> {
        self.remove_empty_if_else_for();

        while let Some(in_expr) = self.input.get(self.ep) {
            self.ep += 1;
            let out_expr = match in_expr {
                &ParseStatement::If(cond) => {
                    self.opt_control_flow(ControlFlow::If(cond));
                    continue;
                }
                &ParseStatement::Else(cond) => {
                    self.opt_control_flow(ControlFlow::Else(cond));
                    continue;
                }
                ParseStatement::ScopeStart => unreachable!(
                    "this should not happen as scopes are consumed by other expressions"
                ),
                &ParseStatement::For { data, binding } => {
                    self.opt_for(data, binding);
                    continue;
                }
                &ParseStatement::View(ident) => {
                    self.output.push(Statement::View(ident));
                    continue;
                }
                &ParseStatement::Node(ident_index) => {
                    let start = self.output.len();

                    // Get attributes and text
                    let mut text_and_attributes = 0;
                    loop {
                        match self.input.get(self.ep) {
                            Some(&ParseStatement::LoadValue(index)) => {
                                self.output.push(Statement::LoadText(index));
                                text_and_attributes += 1;
                                self.ep += 1;
                            }
                            Some(&ParseStatement::LoadAttribute { key, value }) => {
                                self.output.push(Statement::LoadAttribute { key, value });
                                text_and_attributes += 1;
                                self.ep += 1;
                            }
                            _ => break,
                        }
                    }

                    let child_scope_size = match self.input.get(self.ep) {
                        Some(ParseStatement::ScopeStart) => {
                            self.opt_scope();
                            self.output.len() - start - text_and_attributes
                        }
                        _ => 0,
                    };
                    self.output.insert(
                        start,
                        Statement::Node {
                            ident: ident_index,
                            scope_size: child_scope_size,
                        },
                    );
                    continue;
                }
                &ParseStatement::LoadValue(index) => Statement::LoadText(index),
                &ParseStatement::LoadAttribute { key, value } => {
                    Statement::LoadAttribute { key, value }
                }
                &ParseStatement::Declaration {
                    visibility,
                    binding,
                    value,
                } => Statement::Declaration {
                    visibility,
                    binding,
                    value,
                },
                &ParseStatement::Assignment { binding, value } => {
                    Statement::Assignment { binding, value }
                }
                ParseStatement::Eof => continue, // noop, we don't care about EOF
                ParseStatement::ScopeEnd => unreachable!("scopes are consumed by `opt_scope`"),
            };

            self.output.push(out_expr);
        }

        self.output
    }

    fn opt_control_flow(&mut self, control_flow: ControlFlow) {
        let start = self.output.len();
        self.opt_scope();
        let size = self.output.len() - start;
        let expr = match control_flow {
            ControlFlow::If(cond) => Statement::If { cond, size },
            ControlFlow::Else(cond) => Statement::Else { cond, size },
        };
        self.output.insert(start, expr);
    }

    fn opt_scope(&mut self) {
        if let Some(ParseStatement::ScopeStart) = self.input.get(self.ep) {
            self.ep += 1; // consume ScopeStart
        } else {
            panic!(
                "invalid expression: {:?}, opt_scope should only be called on a scope",
                self.input.get(self.ep)
            );
        };

        let start = self.ep;
        let mut end = self.ep;
        let mut level = 1;

        while let Some(expr) = self.input.get(end) {
            match expr {
                ParseStatement::ScopeStart => level += 1,
                ParseStatement::ScopeEnd => {
                    level -= 1;
                    if level == 0 {
                        let input = self.input.drain(start..end).collect::<Vec<_>>();
                        self.ep += 1; // consume the ScopeEnd
                        let mut output = Optimizer::new(input).optimize();
                        self.output.append(&mut output);
                        break;
                    }
                }
                _ => {}
            }
            end += 1;
        }
    }

    fn opt_for(&mut self, data: ValueId, binding: StringId) {
        let start = self.output.len();
        self.opt_scope();
        let end = self.output.len();
        self.output.insert(
            start,
            Statement::For {
                data,
                binding,
                size: end - start,
            },
        );
    }

    fn remove_empty_if_else_for(&mut self) {
        let mut p = 0;
        while let Some(expr) = self.input.get(p) {
            if let ParseStatement::If(_) | ParseStatement::Else(_) | ParseStatement::For { .. } = expr {
                match self.input.get(p + 1) {
                    Some(ParseStatement::ScopeStart) => p += 1,
                    _ => drop(self.input.remove(p)),
                }
            } else {
                p += 1;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parsing::parser::Parser;
    use crate::token::Tokens;
    use crate::{Constants, ViewIds};

    fn parse(src: &str) -> Vec<Statement> {
        let mut consts = Constants::new();
        let mut view_ids = ViewIds::new();
        let lexer = Lexer::new(src, &mut consts);
        let tokens = Tokens::new(lexer.collect::<Result<_, _>>().unwrap(), src.len());
        let parser = Parser::new(tokens, &mut consts, src, &mut view_ids);
        let expr = parser.map(|e| e.unwrap()).collect();
        let opt = Optimizer::new(expr);
        opt.optimize()
    }

    #[test]
    fn optimize_nested_scopes() {
        let src = "
        a
            a
            ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn optimize_if() {
        let src = "
        if a
            a
            ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn optimize_else() {
        let src = "
        if a
            a
        else
            a
            ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Else {
                cond: None,
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn optimize_for() {
        let src = "
        a
        for b in b
            a
            b
            ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::For {
                data: 0.into(),
                binding: 1.into(),
                size: 2
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 1.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn nested_ifs() {
        let src = "
        if a
            if a
                a
            ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 2
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn remove_empty_elses() {
        let src = "
        if x
            a
            a
        else
        if x
            a
        else
        b
        ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 2
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 1.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 1.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 1.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 2.into(),
                scope_size: 0
            }
        );
    }

    #[test]
    fn remove_empty_if() {
        let src = "
        if data
        x
        ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 1.into(),
                scope_size: 0
            }
        );
        assert!(expressions.is_empty());
    }

    #[test]
    fn remove_empty_else() {
        let src = "
            if x
                x
            else
            x
        ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::If {
                cond: 0.into(),
                size: 1
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert!(expressions.is_empty());
    }

    #[test]
    fn optimise_empty_if_else() {
        let src = "
            if x
            else
            x
        ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert!(expressions.is_empty());
    }

    #[test]
    fn optimise_empty_if_else_if() {
        let src = "
            if x
            else if x
            else
            x
        ";
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 0
            }
        );
        assert!(expressions.is_empty());
    }

    #[test]
    fn texts() {
        let src = r#"
            text [a: b] ""
                span ""
        "#;
        let mut expressions = parse(src);
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 0.into(),
                scope_size: 2
            }
        );
        assert_eq!(
            expressions.remove(0),
            Statement::LoadAttribute {
                key: 1.into(),
                value: 0.into()
            }
        );
        assert_eq!(expressions.remove(0), Statement::LoadText(1.into()));
        assert_eq!(
            expressions.remove(0),
            Statement::Node {
                ident: 4.into(),
                scope_size: 0
            }
        );
        assert_eq!(expressions.remove(0), Statement::LoadText(1.into()));
        assert!(expressions.is_empty());
    }
}
