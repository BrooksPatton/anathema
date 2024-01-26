use anathema_values::{Constants, Expression, StringId, ValueId, ViewId, ViewIds, Visibility};

use super::pratt::{eval, expr};
use crate::error::{src_line_no, Error, ErrorKind, Result};
use crate::token::{Kind, Operator, Tokens, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Statement {
    LoadValue(ValueId),
    LoadAttribute {
        key: StringId,
        value: ValueId,
    },
    View(ViewId),
    Node(StringId),
    For {
        data: ValueId,
        binding: StringId,
    },
    Declaration {
        visibility: Visibility,
        binding: StringId,
        value: ValueId,
    },
    Assignment {
        lhs: ValueId,
        rhs: ValueId,
    },
    If(ValueId),
    Else(Option<ValueId>),
    ScopeStart,
    ScopeEnd,
    Eof,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    EnterScope,
    ExitScope,
    ParseFor,
    ParseIf,
    ParseAssignment,
    ParseDeclaration,
    ParseView,
    ParseIdent,
    ParseAttributes,
    ParseAttribute,
    ParseValue,
    Done,
}

// -----------------------------------------------------------------------------
//     - Parser -
// -----------------------------------------------------------------------------
pub struct Parser<'src, 'consts, 'view> {
    tokens: Tokens,
    views: &'view mut ViewIds,
    consts: &'consts mut Constants,
    src: &'src str,
    state: State,
    open_scopes: Vec<usize>,
    closed_scopes: Vec<usize>,
    base_indent: usize,
    done: bool,
}

impl<'src, 'consts, 'view> Parser<'src, 'consts, 'view> {
    pub(crate) fn new(
        mut tokens: Tokens,
        consts: &'consts mut Constants,
        src: &'src str,
        views: &'view mut ViewIds,
    ) -> Self {
        tokens.consume_newlines();
        let base_indent = match tokens.peek() {
            Kind::Indent(indent) => indent,
            _ => 0,
        };

        Self {
            tokens,
            consts,
            views,
            src,
            state: State::EnterScope,
            open_scopes: Vec::new(),
            closed_scopes: Vec::new(),
            base_indent,
            done: false,
        }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        let (line, col) = src_line_no(self.tokens.previous().1, self.src);
        Error {
            line,
            col,
            src: self.src.to_string(),
            kind,
        }
    }

    fn read_ident(&mut self) -> Result<StringId> {
        match self.tokens.next_no_indent() {
            Kind::Value(Value::Ident(ident)) => Ok(ident),
            _ => Err(self.error(ErrorKind::InvalidToken {
                expected: "identifier",
            })),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Statement> {
        // * It is okay to advance the state once and only once
        //   in any parse function using `self.next_state()`.
        //   The exception to this is he parse view function
        // * State should never be set directly in any of the parse functions.
        //   There is one exception of this, and that's when moving from
        //   `ParseAttributes` to `ParseText`.
        loop {
            let output = match self.state {
                State::EnterScope => self.enter_scope(),
                State::ParseFor => self.parse_for(),
                State::ParseIf => self.parse_if(),
                State::ParseDeclaration => self.parse_declaration(),
                State::ParseAssignment => self.parse_assignment(),
                State::ParseView => self.parse_view(),
                State::ExitScope => self.exit_scope(),
                State::ParseIdent => self.parse_ident(),
                State::ParseAttributes => {
                    if !self.parse_attributes()? {
                        self.state = State::ParseValue
                    }
                    Ok(None)
                }
                State::ParseAttribute => self.parse_attribute(),
                State::ParseValue => self.parse_value(),
                State::Done => self.parse_done(),
            };

            match output? {
                Some(inst) => break Ok(inst),
                None => continue,
            }
        }
    }

    fn next_state(&mut self) {
        match self.state {
            State::EnterScope => self.state = State::ExitScope,
            State::ExitScope => self.state = State::ParseFor,
            State::ParseFor => self.state = State::ParseIf,
            State::ParseIf => self.state = State::ParseDeclaration,
            State::ParseDeclaration => self.state = State::ParseAssignment,
            State::ParseAssignment => self.state = State::ParseView,
            State::ParseView => self.state = State::ParseIdent,
            State::ParseIdent => self.state = State::ParseAttributes,
            State::ParseAttributes => self.state = State::ParseAttribute,
            State::ParseAttribute => self.state = State::ParseValue,
            State::ParseValue => self.state = State::Done,
            State::Done => self.state = State::EnterScope,
        }
    }

    // -----------------------------------------------------------------------------
    //     - Stage 1: Parse enter / exit scopes and assignments -
    // -----------------------------------------------------------------------------
    fn enter_scope(&mut self) -> Result<Option<Statement>> {
        let indent = self.tokens.read_indent();

        match self.tokens.peek() {
            Kind::Eof | Kind::Newline => {
                self.next_state();
                return Ok(None);
            }
            _ => {}
        }

        let indent = match indent {
            Some(indent) if indent < self.base_indent => {
                return Err(self.error(ErrorKind::InvalidDedent))
            }
            Some(indent) => Some(indent - self.base_indent),
            None => None,
        };

        let ret = match indent {
            // No indent but open scopes
            None if !self.open_scopes.is_empty() => {
                self.closed_scopes.append(&mut self.open_scopes);
                Ok(None)
            }
            // No indent, no open scopes
            None => Ok(None),
            // Indent
            Some(indent) => match self.open_scopes.last() {
                // Indent is bigger than previous: create another scope
                Some(&last) if indent > last => {
                    self.open_scopes.push(indent);
                    Ok(Some(Statement::ScopeStart))
                }
                // Indent is smaller than previous: close larger scopes
                Some(&last) if indent < last => {
                    if indent > 0 && !self.open_scopes.iter().any(|s| indent.eq(s)) {
                        return Err(self.error(ErrorKind::InvalidDedent));
                    }

                    self.open_scopes.retain(|&s| {
                        if indent < s {
                            self.closed_scopes.push(s);
                            false
                        } else {
                            true
                        }
                    });

                    Ok(None)
                }
                // There are no previous indents, and this indent is not zero
                None if indent > 0 && self.open_scopes.is_empty() => {
                    self.open_scopes.push(indent);
                    Ok(Some(Statement::ScopeStart))
                }
                _ => Ok(None),
            },
        };

        self.next_state();
        ret
    }

    fn exit_scope(&mut self) -> Result<Option<Statement>> {
        match self.closed_scopes.pop() {
            Some(_) => Ok(Some(Statement::ScopeEnd)),
            None => {
                self.next_state();
                Ok(None)
            }
        }
    }

    // -----------------------------------------------------------------------------
    //     - Stage 2: Parse ident, For, If, declaration and assignment -
    // -----------------------------------------------------------------------------
    fn parse_ident(&mut self) -> Result<Option<Statement>> {
        if Kind::Eof == self.tokens.peek() {
            self.state = State::Done;
            return Ok(None);
        }

        // Since the previous parse state was `ParseFor`, the tokens
        // might've been consumed.
        // If the next token is a newline char then move to the next state
        if Kind::Newline == self.tokens.peek() {
            self.next_state();
            return Ok(None);
        }

        let ident = self.read_ident()?;

        self.tokens.consume_indent();
        self.next_state();
        Ok(Some(Statement::Node(ident)))
    }

    fn parse_for(&mut self) -> Result<Option<Statement>> {
        if Kind::For != self.tokens.peek_skip_indent() {
            self.next_state();
            return Ok(None);
        }

        self.tokens.consume();

        let binding = self.read_ident()?;

        if Kind::In != self.tokens.peek_skip_indent() {
            return Err(self.error(ErrorKind::InvalidToken { expected: "in" }));
        }

        // Consume `In`
        self.tokens.consume();

        let expr = expr(&mut self.tokens);
        let value_expr = eval(expr, self.consts);

        // let data = ValueParser::new(&mut self.lexer).parse()?;
        let data = self.consts.store_value(value_expr);

        self.next_state();
        Ok(Some(Statement::For { data, binding }))
    }

    fn parse_if(&mut self) -> Result<Option<Statement>> {
        match self.tokens.peek_skip_indent() {
            Kind::Else => {
                self.tokens.consume();
                let cond = match self.parse_if()? {
                    Some(Statement::If(cond)) => Some(cond),
                    _ => None,
                };

                Ok(Some(Statement::Else(cond)))
            }
            Kind::If => {
                self.tokens.consume();
                let expr = expr(&mut self.tokens);
                let value_expr = eval(expr, self.consts);
                let value_id = self.consts.store_value(value_expr);

                self.next_state();
                Ok(Some(Statement::If(value_id)))
            }
            _ => {
                self.next_state();
                Ok(None)
            }
        }
    }

    fn parse_assignment(&mut self) -> Result<Option<Statement>> {
        // If the current line contains Op::Equal then it's an assignment
        self.next_state();
        if self.tokens.line_contains(Kind::Equal) {
            let lhs = eval(expr(&mut self.tokens), self.consts);

            if let Kind::Equal = self.tokens.peek_skip_indent() {
                self.tokens.consume();
            }

            let rhs = eval(expr(&mut self.tokens), self.consts);
            let lhs = self.consts.store_value(lhs);
            let rhs = self.consts.store_value(rhs);
            Ok(Some(Statement::Assignment { lhs, rhs }))
        } else {
            Ok(None)
        }
    }

    fn parse_declaration(&mut self) -> Result<Option<Statement>> {
        let visibility = match self.tokens.peek_skip_indent() {
            Kind::Local => Visibility::Local,
            Kind::Global => Visibility::Global,
            _ => {
                self.next_state();
                return Ok(None);
            }
        };
        self.tokens.consume();

        let binding = self.read_ident()?;

        if let Kind::Equal = self.tokens.peek_skip_indent() {
            self.tokens.consume();
            let expr = expr(&mut self.tokens);
            let value_expr = eval(expr, self.consts);
            let value = self.consts.store_value(value_expr);
            self.next_state();
            let statement = Statement::Declaration {
                visibility,
                binding,
                value,
            };
            return Ok(Some(statement));
        }

        self.next_state();
        Ok(None)
    }

    fn parse_view(&mut self) -> Result<Option<Statement>> {
        if Kind::View != self.tokens.peek_skip_indent() {
            self.next_state();
            return Ok(None);
        }

        self.tokens.consume();
        self.tokens.consume_indent();

        let ident = self.read_ident()?;
        let ident = self.consts.lookup_string(ident);
        let view_id = self.consts.store_view(self.views, ident.to_owned());
        self.tokens.consume_indent();

        self.next_state();
        self.next_state();
        Ok(Some(Statement::View(view_id)))
    }

    // -----------------------------------------------------------------------------
    //     - Stage 3: Parse attributes -
    // -----------------------------------------------------------------------------
    fn parse_attributes(&mut self) -> Result<bool> {
        if Kind::Op(Operator::LBracket) == self.tokens.peek_skip_indent() {
            self.tokens.consume();
            self.next_state();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // -----------------------------------------------------------------------------
    //     - Stage 4: Parse single attribute -
    // -----------------------------------------------------------------------------
    fn parse_attribute(&mut self) -> Result<Option<Statement>> {
        // Check for the closing bracket
        if Kind::Op(Operator::RBracket) == self.tokens.peek_skip_indent() {
            self.tokens.consume();
            self.next_state();
            return Ok(None);
        }

        let key = self.read_ident()?;

        self.tokens.consume_all_whitespace();

        if Kind::Op(Operator::Colon) != self.tokens.peek_skip_indent() {
            return Err(self.error(ErrorKind::InvalidToken { expected: ":" }));
        }

        self.tokens.consume();
        self.tokens.consume_all_whitespace();

        let expr = expr(&mut self.tokens);
        let value_expr = eval(expr, self.consts);
        let value = self.consts.store_value(value_expr);

        self.tokens.consume_all_whitespace();

        // Consume comma
        if Kind::Op(Operator::Comma) == self.tokens.peek() {
            self.tokens.consume();
            self.tokens.consume_all_whitespace();
        } else if Kind::Op(Operator::RBracket) == self.tokens.peek() {
            self.tokens.consume();
            self.next_state();
        } else {
            return Err(self.error(ErrorKind::UnterminatedAttributes));
        }

        Ok(Some(Statement::LoadAttribute { key, value }))
    }

    // -----------------------------------------------------------------------------
    //     - Stage 5: Node value -
    // -----------------------------------------------------------------------------
    fn parse_value(&mut self) -> Result<Option<Statement>> {
        self.tokens.consume_indent();

        // TODO: does this make sense?
        // Only valid tokens here are:
        // * [
        // * \n
        // * Text
        // * EOF
        // match self.tokens.peek() {
        //     Kind::Newline
        //     | Kind::Value(Value::String(_))
        //     | Kind::Value(Value::Ident(_))
        //     | Kind::Op(Operator::Minus)
        //     | Kind::Op(Operator::LParen)
        //     | Kind::Op(Operator::LBracket)
        //     | Kind::Op(Operator::LCurly)
        //     | Kind::Eof => {}
        //     _ => {
        //         return Err(self.error(ErrorKind::InvalidToken {
        //             expected: "either a new line, `[` or text",
        //         }))
        //     }
        // }

        if matches!(self.tokens.peek(), Kind::Newline | Kind::Eof) {
            self.next_state();
            return Ok(None);
        }

        let mut values = vec![];

        loop {
            if matches!(self.tokens.peek(), Kind::Newline | Kind::Eof) {
                break;
            }
            let expression = expr(&mut self.tokens);
            let value_expr = eval(expression, self.consts);
            values.push(value_expr);
        }

        let value_id = match values.len() {
            0 => panic!("invalid state"),
            1 => self.consts.store_value(values.remove(0)),
            _ => self.consts.store_value(Expression::List(values.into())),
        };

        self.next_state();
        Ok(Some(Statement::LoadValue(value_id)))
    }

    // -----------------------------------------------------------------------------
    //     - Stage 6: Done -
    //     Clear empty spaces, ready for next instructions,
    //     or deal with EOF
    // -----------------------------------------------------------------------------
    fn parse_done(&mut self) -> Result<Option<Statement>> {
        let token = self.tokens.next();

        let ret = match token {
            Kind::Eof if !self.open_scopes.is_empty() => {
                self.open_scopes.pop();
                return Ok(Some(Statement::ScopeEnd));
            }
            Kind::Eof => return Ok(Some(Statement::Eof)),
            Kind::Newline => {
                self.tokens.consume_newlines();
                Ok(None)
            }
            _ => Err(self.error(ErrorKind::InvalidToken {
                expected: "new line",
            })),
        };

        self.next_state();
        ret
    }
}

// -----------------------------------------------------------------------------
//     - Iterator -
// -----------------------------------------------------------------------------
impl Iterator for Parser<'_, '_, '_> {
    type Item = Result<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.parse() {
            Ok(Statement::Eof) => {
                self.done = true;
                Some(Ok(Statement::Eof))
            }
            Err(e) => {
                self.state = State::Done;
                Some(Err(e))
            }
            inst => Some(inst),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(src: &str) -> Vec<Result<Statement>> {
        let mut consts = Constants::new();
        let mut view_ids = ViewIds::new();
        let lexer = Lexer::new(src, &mut consts);
        let tokens = Tokens::new(lexer.collect::<Result<Vec<_>>>().unwrap(), src.len());
        let parser = Parser::new(tokens, &mut consts, src, &mut view_ids);

        parser.collect::<Vec<_>>()
    }

    fn parse_ok(src: &str) -> Vec<Statement> {
        parse(src).into_iter().map(Result::unwrap).collect()
    }

    fn parse_err(src: &str) -> Error {
        parse(src)
            .into_iter()
            .collect::<Result<Vec<_>>>()
            .unwrap_err()
    }

    #[test]
    fn parse_single_instruction() {
        let src = "a";
        let expected = Statement::Node(0.into());
        let actual = parse_ok(src).remove(0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_attributes() {
        let src = "a [a: a]";
        let expected = vec![
            Statement::Node(0.into()),
            Statement::LoadAttribute {
                key: 0.into(),
                value: 0.into(),
            },
            Statement::Eof,
        ];

        let actual = parse_ok(src);
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_text() {
        let src = "a 'a'      \n\n//some comments \n    ";
        let expected = vec![
            Statement::Node(0.into()),
            Statement::LoadValue(0.into()),
            Statement::Eof,
        ];

        let actual = parse_ok(src);
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_scopes() {
        let src = "
            a
                b
                    c
                b
            a
            ";
        let expected = vec![
            Statement::Node(0.into()),
            Statement::ScopeStart,
            Statement::Node(1.into()),
            Statement::ScopeStart,
            Statement::Node(2.into()),
            Statement::ScopeEnd,
            Statement::Node(1.into()),
            Statement::ScopeEnd,
            Statement::Node(0.into()),
            Statement::Eof,
        ];

        let actual = parse_ok(src);
        assert_eq!(expected, actual);

        let src = "
            a
                b
                    c
            ";
        let expected = vec![
            Statement::Node(0.into()),
            Statement::ScopeStart,
            Statement::Node(1.into()),
            Statement::ScopeStart,
            Statement::Node(2.into()),
            Statement::ScopeEnd,
            Statement::ScopeEnd,
            Statement::Eof,
        ];

        let actual = parse_ok(src);
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_nested_for_loops() {
        let src = "
        x
            for x in data
                for y in data
                    x
        ";
        let mut instructions = parse_ok(src);

        assert_eq!(instructions.remove(0), Statement::Node(0.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(
            instructions.remove(0),
            Statement::For {
                data: 0.into(),
                binding: 0.into()
            }
        );
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(
            instructions.remove(0),
            Statement::For {
                data: 0.into(),
                binding: 2.into()
            }
        );
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(0.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
    }

    #[test]
    fn parse_scopes_and_for() {
        let src = "
        x
            y
        for x in data
            y
        ";
        let mut instructions = parse_ok(src);
        assert_eq!(instructions.remove(0), Statement::Node(0.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(1.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
        assert_eq!(
            instructions.remove(0),
            Statement::For {
                data: 0.into(),
                binding: 0.into()
            }
        );
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(1.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
    }

    #[test]
    fn parse_if() {
        let src = "
        if data
            x
        ";
        let mut instructions = parse_ok(src);

        assert_eq!(instructions.remove(0), Statement::If(0.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(1.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
    }

    #[test]
    fn parse_else() {
        let src = "
        if data
            x
        else
            y
        ";
        let mut instructions = parse_ok(src);

        assert_eq!(instructions.remove(0), Statement::If(0.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(1.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
        assert_eq!(instructions.remove(0), Statement::Else(None));
        assert_eq!(instructions.remove(0), Statement::ScopeStart);
        assert_eq!(instructions.remove(0), Statement::Node(2.into()));
        assert_eq!(instructions.remove(0), Statement::ScopeEnd);
    }

    #[test]
    fn parse_if_else_if_else() {
        let src = "
        if data
            x
        else if data
            y
        else
            z
        ";
        let mut expressions = parse_ok(src);

        assert_eq!(expressions.remove(0), Statement::If(0.into()));
        assert_eq!(expressions.remove(0), Statement::ScopeStart);
        assert_eq!(expressions.remove(0), Statement::Node(1.into()));
        assert_eq!(expressions.remove(0), Statement::ScopeEnd);
        assert_eq!(expressions.remove(0), Statement::Else(Some(0.into())));
        assert_eq!(expressions.remove(0), Statement::ScopeStart);
        assert_eq!(expressions.remove(0), Statement::Node(2.into()));
        assert_eq!(expressions.remove(0), Statement::ScopeEnd);
        assert_eq!(expressions.remove(0), Statement::Else(None));
        assert_eq!(expressions.remove(0), Statement::ScopeStart);
        assert_eq!(expressions.remove(0), Statement::Node(3.into()));
        assert_eq!(expressions.remove(0), Statement::ScopeEnd);
    }

    #[test]
    fn parse_view() {
        let src = "@mail";
        let mut expressions = parse_ok(src);
        assert_eq!(expressions.remove(0), Statement::View(0.into()));

        let src = "@mail state";
        let mut expressions = parse_ok(src);
        assert_eq!(expressions.remove(0), Statement::View(0.into()));
        assert_eq!(expressions.remove(0), Statement::LoadValue(0.into()));
    }

    #[test]
    fn parse_empty_if() {
        let src = "
            if x
            x
        ";

        let mut expressions = parse_ok(src);
        assert_eq!(expressions.remove(0), Statement::If(0.into()));
        assert_eq!(expressions.remove(0), Statement::Node(0.into()));
    }

    #[test]
    fn parse_no_instruction() {
        let src = "";
        let expected: Vec<Statement> = vec![Statement::Eof];
        let actual = parse_ok(src);
        assert_eq!(expected, actual);

        let src = "\n// comment         \n";
        let expected: Vec<Statement> = vec![Statement::Eof];
        let actual = parse_ok(src);
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_text_with_multiple_values() {
        let src = "a 'a' 'b'";
        let mut expressions = parse_ok(src);
        assert_eq!(expressions.remove(0), Statement::Node(0.into()));
        assert_eq!(expressions.remove(0), Statement::LoadValue(0.into()));
    }

    #[test]
    fn parse_declaration() {
        let src = "local x = 1";
        let mut expressions = parse_ok(src);
        assert!(matches!(
            expressions.remove(0),
            Statement::Declaration { .. }
        ));
    }

    #[test]
    fn parse_ident_assignment() {
        let src = "x = 1";
        let mut expressions = parse_ok(src);
        assert!(matches!(
            expressions.remove(0),
            Statement::Assignment { .. }
        ));
    }

    #[test]
    fn parse_complex_assignment() {
        let src = "x[1]['omg'] = x[2]";
        let mut expressions = parse_ok(src);
        assert!(matches!(
            expressions.remove(0),
            Statement::Assignment { .. }
        ));
    }

    #[test]
    fn parse_invalid_declaration() {
        let src = "local x = global y = 1";
        let err = parse_err(src);
    }

    #[test]
    fn multi_line_assignment() {
        let src = "
        x[1]['omg'] = {
            'a': 1,
            'b': {
                'a': 2,
            },
        }";
        let mut expressions = parse_ok(src);
        assert!(matches!(
            expressions.remove(0),
            Statement::Assignment { .. }
        ));
    }

}
