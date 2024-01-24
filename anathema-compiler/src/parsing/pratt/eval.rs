use std::rc::Rc;

use anathema_values::hashmap::HashMap;
use anathema_values::{Constants, Num, Owned, Expression, Visibility};

use super::Expr;
use crate::token::Operator;

pub fn eval(expr: Expr, consts: &Constants) -> Expression {
    match expr {
        Expr::Bool(b) => Expression::from(b),
        Expr::Color(color) => Expression::from(color),
        Expr::Ident(string_id) => {
            let string = consts.lookup_string(string_id);
            Expression::Ident(string.into())
        }
        Expr::Str(string_id) => {
            let string = consts.lookup_string(string_id);
            Expression::String(Rc::from(string))
        }
        Expr::Num(num) => Expression::Owned(Owned::Num(num.into())),
        Expr::Float(num) => Expression::Owned(Owned::Num(Num::Float(num))),
        Expr::Array { lhs, index } => {
            let lhs = eval(*lhs, consts);
            let index = eval(*index, consts);
            Expression::Index(lhs.into(), index.into())
        }
        Expr::Binary { op, lhs, rhs } => match op {
            Operator::Dot => Expression::Dot(eval(*lhs, consts).into(), eval(*rhs, consts).into()),
            Operator::Mul | Operator::Plus | Operator::Minus | Operator::Div | Operator::Mod => {
                let (lhs, rhs) = match (eval(*lhs, consts), eval(*rhs, consts)) {
                    (Expression::Owned(Owned::Num(lhs)), Expression::Owned(Owned::Num(rhs))) => {
                        match op {
                            Operator::Mul => return Expression::Owned(Owned::Num(lhs * rhs)),
                            Operator::Plus => return Expression::Owned(Owned::Num(lhs + rhs)),
                            Operator::Minus => return Expression::Owned(Owned::Num(lhs - rhs)),
                            Operator::Div => return Expression::Owned(Owned::Num(lhs / rhs)),
                            Operator::Mod => return Expression::Owned(Owned::Num(lhs % rhs)),
                            _ => unreachable!(),
                        }
                    }
                    (lhs, rhs) => (lhs.into(), rhs.into()),
                };

                match op {
                    Operator::Mul => Expression::Mul(lhs, rhs),
                    Operator::Plus => Expression::Add(lhs, rhs),
                    Operator::Minus => Expression::Sub(lhs, rhs),
                    Operator::Div => Expression::Div(lhs, rhs),
                    Operator::Mod => Expression::Mod(lhs, rhs),
                    _ => unreachable!(),
                }
            }
            Operator::EqualEqual => {
                Expression::Equality(eval(*lhs, consts).into(), eval(*rhs, consts).into())
            }
            Operator::GreaterThan => {
                Expression::Greater(eval(*lhs, consts).into(), eval(*rhs, consts).into())
            }
            Operator::GreaterThanOrEqual => {
                Expression::GreaterEqual(eval(*lhs, consts).into(), eval(*rhs, consts).into())
            }
            Operator::LessThan => {
                Expression::Less(eval(*lhs, consts).into(), eval(*rhs, consts).into())
            }
            Operator::LessThanOrEqual => {
                Expression::LessEqual(eval(*lhs, consts).into(), eval(*rhs, consts).into())
            }
            Operator::Or | Operator::And => {
                let lhs = eval(*lhs, consts);
                let rhs = eval(*rhs, consts);
                match op {
                    Operator::Or => Expression::Or(lhs.into(), rhs.into()),
                    Operator::And => Expression::And(lhs.into(), rhs.into()),
                    _ => unreachable!(),
                }
            }
            e => panic!("here is a panic: {e:#?}"),
        },
        Expr::Unary { op, expr } => {
            let expr = eval(*expr, consts);

            match op {
                Operator::Not => match expr {
                    Expression::Owned(Owned::Bool(b)) => Expression::Owned((!b).into()),
                    _ => Expression::Not(expr.into()),
                },
                Operator::Minus => match expr {
                    Expression::Owned(Owned::Num(Num::Unsigned(n))) => {
                        Expression::Owned(Owned::Num(Num::Signed(-(n as i64))))
                    }
                    _ => Expression::Negative(expr.into()),
                },
                _ => panic!("operator: {op:#?}"),
            }
        }
        Expr::List(list) => {
            Expression::List(list.into_iter().map(|expr| eval(expr, consts)).collect())
        }
        Expr::Map(map) => Expression::Map(
            map.into_iter()
                .map(|(key, value)| (eval(key, consts).to_string(), eval(value, consts)))
                .collect::<HashMap<_, _>>()
                .into(),
        ),
        Expr::Call { fun, args } => {
            let args = args.into_iter().map(|expr| eval(expr, consts)).collect();
            Expression::Call {
                fun: eval(*fun, consts).into(),
                args,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parsing::pratt::expr;
    use crate::token::Tokens;
    use crate::Constants;

    fn eval_str(input: &str) -> Expression {
        let mut consts = Constants::new();
        let lexer = Lexer::new(input, &mut consts);
        let tokens = lexer.collect::<Result<_, _>>().unwrap();
        let mut tokens = Tokens::new(tokens, input.len());

        let expression = expr(&mut tokens);
        eval(expression, &consts)
    }

    #[test]
    fn ident() {
        let expr = eval_str("ident");
        assert_eq!(expr.to_string(), "ident");
    }

    #[test]
    fn index() {
        let expr = eval_str("a[x]");
        assert_eq!(expr.to_string(), "a[x]");
    }

    #[test]
    fn number() {
        let expr = eval_str("123");
        assert_eq!(expr.to_string(), "123");
    }

    #[test]
    fn negative_number() {
        let expr = eval_str("-123");
        assert_eq!(expr.to_string(), "-123");
    }

    #[test]
    fn lookup() {
        let expr = eval_str("a.b.c");
        assert_eq!(expr.to_string(), "a.b.c");
    }

    #[test]
    fn bool() {
        let expr = eval_str("true");
        assert_eq!(expr.to_string(), "true");

        let expr = eval_str("!true");
        assert_eq!(expr.to_string(), "false");

        let expr = eval_str("!false");
        assert_eq!(expr.to_string(), "true");

        let expr = eval_str("!!false");
        assert_eq!(expr.to_string(), "false");

        let expr = eval_str("!hello");
        assert_eq!(expr.to_string(), "!hello");

        let expr = eval_str("!!hello");
        assert_eq!(expr.to_string(), "!!hello");
    }

    #[test]
    fn strings() {
        let expr = eval_str("'single quote'");
        assert_eq!(expr.to_string(), "single quote");

        let expr = eval_str("\"double quote\"");
        assert_eq!(expr.to_string(), "double quote");
    }

    #[test]
    fn addition() {
        let expr = eval_str("-2 + -3");
        assert_eq!(expr.to_string(), "-5");

        let expr = eval_str("2 + -3");
        assert_eq!(expr.to_string(), "-1");

        let expr = eval_str("2 + -1");
        assert_eq!(expr.to_string(), "1");

        let expr = eval_str("-3 + 2");
        assert_eq!(expr.to_string(), "-1");

        let expr = eval_str("-1 + 2");
        assert_eq!(expr.to_string(), "1");

        let expr = eval_str("1 + 2 * 3");
        assert_eq!(expr.to_string(), "7");

        let expr = eval_str("a + b * c");
        assert_eq!(expr.to_string(), "a + b * c");
    }

    #[test]
    fn multiplication() {
        let expr = eval_str("2 * 2");
        assert_eq!(expr.to_string(), "4");

        let expr = eval_str("x * 2 * 2");
        assert_eq!(expr.to_string(), "x * 2 * 2");
    }

    #[test]
    fn subtraction() {
        let expr = eval_str("5 - 4");
        assert_eq!(expr.to_string(), "1");

        let expr = eval_str("-5 - 4");
        assert_eq!(expr.to_string(), "-9");

        let expr = eval_str("-5 - -4");
        assert_eq!(expr.to_string(), "-1");

        let expr = eval_str("a - b");
        assert_eq!(expr.to_string(), "a - b");
    }

    #[test]
    fn division() {
        let expr = eval_str("5 / 4");
        assert_eq!(expr.to_string(), "1");

        let expr = eval_str("a / b");
        assert_eq!(expr.to_string(), "a / b");

        let expr = eval_str("-a / b");
        assert_eq!(expr.to_string(), "-a / b");
    }

    #[test]
    fn modulo() {
        let expr = eval_str("5 % 4");
        assert_eq!(expr.to_string(), "1");

        let expr = eval_str("a % 4");
        assert_eq!(expr.to_string(), "a % 4");
    }

    #[test]
    fn function_call() {
        let expr = eval_str("fun(5, 4)");
        assert_eq!(expr.to_string(), "fun(5, 4)");
    }
}
