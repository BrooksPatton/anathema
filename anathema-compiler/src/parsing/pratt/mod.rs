use std::fmt::Display;

use anathema_render::Color;
use anathema_values::StringId;

pub use self::eval::eval;
use crate::token::{Kind, Operator, Tokens, Value};

mod eval;

pub mod prec {
    pub const INITIAL: u8 = 0;
    pub const ASSIGNMENT: u8 = 1;
    pub const CONDITIONAL: u8 = 2;
    pub const LOGICAL: u8 = 3;
    pub const SUM: u8 = 4;
    pub const PRODUCT: u8 = 5;
    pub const PREFIX: u8 = 7;
    pub const CALL: u8 = 9;
    pub const SUBCRIPT: u8 = 10;
}

fn get_precedence(op: Operator) -> u8 {
    match op {
        Operator::Equal => prec::ASSIGNMENT,
        Operator::GreaterThan
        | Operator::GreaterThanOrEqual
        | Operator::LessThan
        | Operator::LessThanOrEqual => prec::LOGICAL,
        Operator::Or | Operator::And | Operator::EqualEqual => prec::CONDITIONAL,
        Operator::Plus | Operator::Minus => prec::SUM,
        Operator::Mul | Operator::Div | Operator::Mod => prec::PRODUCT,
        Operator::LParen => prec::CALL,
        Operator::Dot => prec::SUBCRIPT,
        Operator::LBracket => prec::SUBCRIPT,
        _ => 0,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unary {
        op: Operator,
        expr: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Operator,
    },
    Bool(bool),
    Num(u64),
    Float(f64),
    Color(Color),
    Ident(StringId),
    Str(StringId),
    Call {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    Local {
        ident: StringId,
        value: Box<Expr>,
    },
    Global {
        ident: StringId,
        value: Box<Expr>,
    },
    Array {
        lhs: Box<Expr>,
        index: Box<Expr>,
    },
    List(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary { op, expr } => write!(f, "({op}{expr})"),
            Expr::Binary { op, lhs, rhs } => write!(f, "({op} {lhs} {rhs})"),
            Expr::Bool(b) => write!(f, "{b}"),
            Expr::Num(b) => write!(f, "{b}"),
            Expr::Float(b) => write!(f, "{b}"),
            Expr::Color(color) => write!(f, "{color:?}"),
            Expr::Ident(sid) => write!(f, "{sid}"),
            Expr::Str(sid) => write!(f, "\"{sid}\""),
            Expr::Array { lhs, index } => write!(f, "{lhs}[{index}]"),
            Expr::List(list) => {
                let s = list
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{s}]")
            }
            Expr::Map(map) => {
                let s = map
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{s}}}")
            }
            Expr::Call { fun, args } => {
                let s = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{fun}({s})")
            }
            Expr::Local { ident, value } => write!(f, "local {ident} = {value}"),
            Expr::Global { ident, value } => write!(f, "global {ident} = {value}"),
        }
    }
}

pub(crate) fn expr(tokens: &mut Tokens) -> Expr {
    expr_bp(tokens, 0)
}

// TODO: add error handling here,
//       as some of these experssions can fail
fn expr_bp(tokens: &mut Tokens, precedence: u8) -> Expr {
    let mut left = match tokens.next_no_indent() {
        Kind::Op(Operator::LBracket) => parse_collection(tokens),
        Kind::Op(Operator::LCurly) => parse_map(tokens),
        Kind::Op(Operator::LParen) => {
            let left = expr_bp(tokens, prec::INITIAL);
            // Need to consume the closing bracket
            assert!(matches!(
                tokens.next_no_indent(),
                Kind::Op(Operator::RParen)
            ));
            left
        }
        Kind::Local => match expr_bp(tokens, precedence) {
            Expr::Binary {
                lhs,
                rhs,
                op: Operator::Equal,
            } => match *lhs {
                Expr::Ident(ident) => return Expr::Local { ident, value: rhs },
                _ => panic!("invalid identifier"),
            },
            w => panic!("{w:#?}"),
            _ => panic!("invalid declaration"),
        },
        Kind::Global => match expr_bp(tokens, precedence) {
            Expr::Binary {
                lhs,
                rhs,
                op: Operator::Equal,
            } => match *lhs {
                Expr::Ident(ident) => return Expr::Global { ident, value: rhs },
                _ => panic!("invalid identifier"),
            },
            w => panic!("{w:#?}"),
            _ => panic!("invalid declaration"),
        },
        Kind::Op(op) => Expr::Unary {
            op,
            expr: Box::new(expr_bp(tokens, prec::PREFIX)),
        },
        Kind::Value(value) => match value {
            Value::Number(n) => Expr::Num(n),
            Value::Float(n) => Expr::Float(n),
            Value::Ident(ident) => Expr::Ident(ident),
            Value::String(sid) => Expr::Str(sid),
            Value::Bool(b) => Expr::Bool(b),
            Value::Color(color) => Expr::Color(color),
        },
        Kind::Eof => panic!("unexpected eof"),
        // TODO: see panic
        kind => panic!("we'll deal with this later: {kind:#?}"),
    };

    loop {
        // This could be EOF, which is fine.
        // It could also be any other token which would be
        // a syntax error, but I don't mind that just now
        let Kind::Op(op) = tokens.peek_skip_indent() else {
            return left;
        };

        let token_prec = get_precedence(op);

        // If the current token precedence is higher than the current precedence, then we bind to the right,
        // otherwise we bind to the left
        if precedence >= token_prec {
            break;
        }

        tokens.consume();

        // Postfix parsing
        match op {
            Operator::LParen => {
                left = parse_function(tokens, left);
                continue;
            }
            Operator::LBracket => {
                left = Expr::Array {
                    lhs: Box::new(left),
                    index: Box::new(expr_bp(tokens, prec::INITIAL)),
                };
                let next_token = tokens.next_no_indent();
                let Kind::Op(Operator::RBracket) = next_token else {
                    panic!("invalid token");
                };
                continue;
            }
            _ => {}
        }

        let right = expr_bp(tokens, token_prec);
        left = Expr::Binary {
            lhs: Box::new(left),
            op,
            rhs: Box::new(right),
        };

        continue;
    }

    left
}

fn parse_function(tokens: &mut Tokens, left: Expr) -> Expr {
    let mut args = vec![];

    loop {
        match tokens.peek_skip_indent() {
            Kind::Op(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            Kind::Op(Operator::RParen) => {
                tokens.consume();
                break;
            }
            _ => (),
        }
        args.push(expr_bp(tokens, prec::INITIAL));
    }

    Expr::Call {
        fun: Box::new(left),
        args,
    }
}

fn parse_collection(tokens: &mut Tokens) -> Expr {
    let mut elements = vec![];

    loop {
        match tokens.peek_skip_indent() {
            Kind::Op(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            Kind::Op(Operator::RBracket) => {
                tokens.consume();
                break;
            }
            _ => (),
        }
        elements.push(expr_bp(tokens, prec::INITIAL));
    }

    Expr::List(elements)
}

fn parse_map(tokens: &mut Tokens) -> Expr {
    let mut elements = vec![];

    loop {
        match tokens.peek_skip_indent() {
            Kind::Op(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            Kind::Op(Operator::RCurly) => {
                tokens.consume();
                break;
            }
            _ => (),
        }

        let key = expr_bp(tokens, prec::INITIAL);

        match tokens.peek_skip_indent() {
            Kind::Op(Operator::Colon) => tokens.consume(),
            _ => break,
        }

        let value = expr_bp(tokens, prec::INITIAL);
        elements.push((key, value));
    }

    Expr::Map(elements)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::Result;
    use crate::lexer::Lexer;
    use crate::Constants;

    fn parse_expr(input: &str) -> Expr {
        let mut consts = Constants::new();
        let lexer = Lexer::new(input, &mut consts);
        let tokens = lexer.collect::<Result<_>>().unwrap();
        let mut tokens = Tokens::new(tokens, input.len());

        expr(&mut tokens)
    }

    fn parse(input: &str) -> String {
        let expression = parse_expr(input);
        expression.to_string()
    }

    #[test]
    fn add_sub() {
        let input = "1 + 2";
        assert_eq!(parse(input), "(+ 1 2)");

        let input = "1 - 2";
        assert_eq!(parse(input), "(- 1 2)");
    }

    #[test]
    fn mul_div() {
        let input = "5 + 1 * 2";
        assert_eq!(parse(input), "(+ 5 (* 1 2))");

        let input = "5 - 1 / 2";
        assert_eq!(parse(input), "(- 5 (/ 1 2))");
    }

    #[test]
    fn brackets() {
        let input = "(5 + 1) * 2";
        assert_eq!(parse(input), "(* (+ 5 1) 2)");
    }

    #[test]
    fn function() {
        let input = "fun(1, a + 2 * 3, 3)";
        assert_eq!(parse(input), "<sid 0>(1, (+ <sid 1> (* 2 3)), 3)");
    }

    #[test]
    fn function_no_args() {
        let input = "f()";
        assert_eq!(parse(input), "<sid 0>()");
    }

    #[test]
    fn array_index() {
        let input = "array[0][1]";
        assert_eq!(parse(input), "<sid 0>[0][1]");
    }

    #[test]
    fn map_lookup() {
        let input = "map['key']";
        assert_eq!(parse(input), "<sid 0>[\"<sid 1>\"]");
    }

    #[test]
    fn dot_lookup() {
        let input = "a.b.c";
        assert_eq!(parse(input), "(. (. <sid 0> <sid 1>) <sid 2>)");
    }

    #[test]
    fn modulo() {
        let input = "5 + 1 % 2";
        assert_eq!(parse(input), "(+ 5 (% 1 2))");
    }

    #[test]
    fn list() {
        let input = "[1, 2, a, 4]";
        assert_eq!(parse(input), "[1, 2, <sid 0>, 4]");
    }

    #[test]
    fn nested_list() {
        let input = "[1, [2, 3, [4, 5]]]";
        assert_eq!(parse(input), "[1, [2, 3, [4, 5]]]");
    }

    #[test]
    fn equality() {
        let input = "1 == 2";
        assert_eq!(parse(input), "(== 1 2)");
    }

    #[test]
    fn map() {
        let input = "{a: 1, b: c}";
        assert_eq!(parse(input), "{<sid 0>: 1, <sid 1>: <sid 2>}");
    }

    #[test]
    fn global_declaration() {
        let input = "global key = val + 1";
        let output = parse_expr(input);
        assert_eq!(parse(input), "global <sid 0> = (+ <sid 1> 1)");
    }

    #[test]
    fn local_declaration() {
        let input = "local key = val";
        let output = parse_expr(input);
        assert_eq!(parse(input), "local <sid 0> = <sid 1>");
    }

    #[test]
    fn ident_assignment() {
        let input = "key = val";
        assert_eq!(parse(input), "(= <sid 0> <sid 1>)");
    }

    #[test]
    fn map_assignment() {
        let input = "key['lol'] = flopp";
        assert_eq!(parse(input), "(= <sid 0>[\"<sid 1>\"] <sid 2>)");
    }
}
