use std::collections::HashMap;

use super::Context;
use crate::expressions::{Expression, Op};
use crate::primitives::Primitive;

// Evaluate the expression using `vars` as a backing store.
// e.g `a.b.c` would first find `a` in `vars` and resolve the remaining path
// from within that expression.
//
// ```
// a.b[c]
// ```
// would resolve `a` from vars, `b` from `a`, and `c` from vars.
fn eval_path(expr: &Expression, ctx: &Context<'_, '_>) -> Option<Expression> {
    use {Expression as E, Primitive as P};

    match expr {
        E::Ident(ident) => {
            let ident: &str = ident;
            ctx.fetch(ident)
        }
        E::Index(lhs, index) => match eval_path(lhs, ctx)? {
            E::List(list) => match const_eval(index.clone(), ctx) {
                E::Primitive(P::Int(num)) => list.get(num as usize).cloned(),
                _ => Some(E::Index(
                    E::List(list.clone()).into(),
                    const_eval(*index.clone(), ctx).into(),
                )),
            },
            E::Map(map) => match const_eval(index.clone(), ctx) {
                E::Str(key) => map.get(&*key).cloned(),
                E::Ident(key) => match ctx.fetch(&key)? {
                    E::Str(key) => map.get(&*key).cloned().map(|e| const_eval(e, ctx)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        E::Dot(lhs, rhs) => match eval_path(lhs, ctx)? {
            E::Map(map) => match &**rhs {
                E::Ident(key) => map.get(&**key).cloned().map(|e| const_eval(e, ctx)),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn const_eval(expr: impl Into<Expression>, ctx: &Context<'_, '_>) -> Expression {
    use {Expression as E, Primitive as P};

    macro_rules! ce {
        ($e:expr) => {
            const_eval($e, ctx).into()
        };
    }

    let expr = expr.into();
    match expr {
        expr @ (E::Primitive(_) | E::Str(_)) => expr,
        E::Not(expr) => E::Not(ce!(*expr)),
        E::Negative(expr) => E::Negative(ce!(*expr)),
        E::Equality(lhs, rhs, eq) => E::Equality(ce!(*lhs), ce!(*rhs), eq),

        E::Ident(_) => eval_path(&expr, ctx).map(|e| ce!(e)).unwrap_or(expr),
        E::Dot(..) => eval_path(&expr, ctx).map(|e| ce!(e)).unwrap_or(expr),
        E::Index(..) => eval_path(&expr, ctx).map(|e| ce!(e)).unwrap_or(expr),

        E::List(list) => {
            let list = list.iter().cloned().map(|expr| ce!(expr)).collect();
            E::List(list)
        }
        E::Map(map) => {
            let hm = HashMap::from_iter(map.iter().map(|(k, v)| (k.clone(), ce!(v.clone()))));
            E::Map(hm.into())
        }
        E::Op(lhs, rhs, op) => match (ce!(*lhs), ce!(*rhs)) {
            (E::Primitive(P::Int(lhs)), E::Primitive(P::Int(rhs))) => {
                let val = match op {
                    Op::Add => lhs + rhs,
                    Op::Sub => lhs - rhs,
                    Op::Div => lhs / rhs,
                    Op::Mul => lhs * rhs,
                    Op::Mod => lhs % rhs,
                };
                E::Primitive(P::Int(val))
            }
            (lhs, rhs) => E::Op(lhs.into(), rhs.into(), op),
        },
        E::Call { fun, args } => E::Call {
            fun: fun.clone(),
            args: args.iter().map(|expr| ce!(expr.clone())).collect(),
        },
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expressions::{add, dot, ident, index, list, map, num, strlit};
    use crate::statements::with_context;

    #[test]
    fn declare_twice_const_folding() {
        with_context(|ctx| {
            ctx.locals.declare("a", num(1));
            ctx.locals.declare("b", ident("a"));
            let output = const_eval(ident("b"), &ctx);
            assert_eq!(*num(1), output);
        });
    }

    #[test]
    fn eval_map() {
        with_context(|ctx| {
            ctx.locals.declare("a", map([("b", ident("c"))]));
            ctx.locals.declare("c", num(1));
            let expr = dot(ident("a"), ident("b"));
            let output = const_eval(expr, &ctx);
            assert_eq!(*num(1), output);
        });
    }

    #[test]
    fn eval_index() {
        // a.b = [1]
        // c = 0
        with_context(|ctx| {
            ctx.locals.declare("a", map([("b", list([num(1)]))]));
            ctx.locals.declare("c", num(0));
            let expr = index(dot(ident("a"), ident("b")), ident("c"));
            let output = const_eval(expr, &ctx);
            assert_eq!(*num(1), output);
        });
    }

    #[test]
    fn list_const_eval() {
        with_context(|ctx| {
            ctx.locals.declare("a", list([strlit("red"), strlit("blue")]));
            ctx.locals.declare("b", index(ident("a"), num(1)));
            let output = const_eval(ident("b"), &ctx);
            assert_eq!(*strlit("blue"), output);
        });
    }

    #[test]
    fn list_dyn_eval() {
        with_context(|ctx| {
            ctx.locals.declare("a", list([strlit("red"), strlit("blue")]));
            ctx.locals.declare("b", num(1));
            let expr = index(ident("a"), add(ident("some_state"), ident("b")));
            let expected = *index(list([strlit("red"), strlit("blue")]), add(ident("some_state"), num(1)));
            let output = const_eval(expr, &ctx);
            assert_eq!(expected, output);
        });
    }

    #[test]
    fn global_dec() {
        with_context(|ctx| {
            ctx.globals.declare("a", list([strlit("red"), strlit("blue")]));
            ctx.locals.declare("b", num(1));
            let expr = *index(ident("a"), ident("b"));
            let output = const_eval(expr, &ctx);
            assert_eq!(output, *strlit("blue"));
        });
    }
}