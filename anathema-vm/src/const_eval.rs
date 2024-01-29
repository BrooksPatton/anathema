use anathema_values::hashmap::HashMap;
use anathema_values::{Expression, Owned as Own, Variables};

// Evaluate the expression using `vars` as a backing store.
// e.g `a.b.c` would first find `a` in `vars` and resolve the remaining path
// from within that expression.
// 
// ```
// a.b[c]
// ```
// would resolve `a` from vars, `b` from `a`, and `c` from vars.
fn eval_dot<'a>(expr: &'a Expression, vars: &'a Variables) -> Option<&'a Expression> {
    match expr {
        Expression::Ident(ident) => {
            let ident: &str = &*ident;
            vars.fetch(ident)
        }
        Expression::Index(lhs, index) => match eval_dot(lhs, vars)? {
            Expression::List(list) => match &**index {
                Expression::Owned(Own::Num(num)) => list.get(num.to_usize()),
                Expression::Ident(ident) => match vars.fetch(ident)? {
                    Expression::Owned(Own::Num(num)) => list.get(num.to_usize()),
                    _ => None,
                },
                _ => None,
            },
            Expression::Map(map) => match &**index {
                Expression::Str(key) => map.get(&**key),
                Expression::Ident(key) => match vars.fetch(key)? {
                    Expression::Str(key) => map.get(&**key),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        Expression::Dot(lhs, rhs) => match eval_dot(lhs, vars)? {
            Expression::Map(map) => match &**rhs {
                Expression::Ident(key) => map.get(&**key),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn eval_dot_mut<'a>(expr: &Expression, vars: &'a mut Variables) -> Option<&'a mut Expression> {
    // 1. Get the root mutably,
    // 2. Proceed from the root
    //
    // Ident a -> get(a) 
    // Index a 1 -> get(a) -> get(1)
    // Map   a.b -> get(a) -> get(b)

    // TODO
    // current issue is that we need to be able to resolve values while having mutable
    // access to the value store (yikes!).
    //
    // e.g
    // assign with a[b]
    // 
    // this needs to lookup `b` while having mutable
    // access to `a`

    match expr {
        Expression::Ident(ident) => {
            let ident: &str = &*ident;
            vars.fetch_mut(ident)
        }
    //     Expression::Index(lhs, index) => match eval_dot_mut(lhs, vars)? {
    //         Expression::List(list) => match &**index {
    //             Expression::Owned(Own::Num(num)) => list.get_mut(num.to_usize()),
    //             Expression::Ident(ident) => match vars.fetch_mut(ident)? {
    //                 Expression::Owned(Own::Num(num)) => list.get_mut(num.to_usize()),
    //                 _ => None,
    //             },
    //             _ => None,
    //         },
    //         Expression::Map(map) => match &**index {
    //             Expression::Str(key) => map.get_mut(&**key),
    //             Expression::Ident(key) => match vars.fetch_mut(key)? {
    //                 Expression::Str(key) => map.get_mut(&**key),
    //                 _ => None,
    //             },
    //             _ => None,
    //         },
    //         _ => None,
    //     },
    //     Expression::Dot(lhs, rhs) => match eval_dot(lhs, vars)? {
    //         Expression::Map(map) => match &**rhs {
    //             Expression::Ident(key) => map.get_mut(&**key),
    //             _ => None,
    //         },
    //         _ => None,
    //     },
        _ => None,
    }
}

pub(crate) fn const_eval(expr: &Expression, vars: &Variables) -> Expression {
    use Expression::*;

    macro_rules! ce {
        ($e:expr) => {
            const_eval($e, vars).into()
        };
    }

    match expr {
        expr @ (Owned(_) | Str(_) | Index(_, _)) => expr.clone(),
        Not(expr) => Not(ce!(expr)),
        Negative(expr) => Negative(ce!(expr)),
        And(lhs, rhs) => And(ce!(lhs), ce!(rhs)),
        Or(lhs, rhs) => Or(ce!(lhs), ce!(rhs)),
        Equality(lhs, rhs) => Equality(ce!(lhs), ce!(rhs)),
        Greater(lhs, rhs) => Greater(ce!(lhs), ce!(rhs)),
        GreaterEqual(lhs, rhs) => GreaterEqual(ce!(lhs), ce!(rhs)),
        Less(lhs, rhs) => Less(ce!(lhs), ce!(rhs)),
        LessEqual(lhs, rhs) => LessEqual(ce!(lhs), ce!(rhs)),

        expr @ Ident(ident) => match vars.fetch(ident) {
            Some(expr) => ce!(expr),
            None => expr.clone(),
        },
        expr @ Dot(..) => match eval_dot(&expr, vars) {
            Some(expr) => expr.clone(),
            None => expr.clone(),
        },
        List(list) => {
            let list = list.into_iter().map(|expr| ce!(expr)).collect();
            List(list)
        }
        Map(map) => {
            let hm = HashMap::from_iter(map.into_iter().map(|(k, v)| (k.clone(), ce!(v))));
            Map(hm.into())
        }

        Add(lhs, rhs) => match (ce!(lhs), ce!(rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs + rhs).into())),
            (lhs, rhs) => Add(lhs.into(), rhs.into()),
        },
        Sub(lhs, rhs) => match (ce!(lhs), ce!(rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs - rhs).into())),
            (lhs, rhs) => Sub(lhs.into(), rhs.into()),
        },
        Div(lhs, rhs) => match (ce!(lhs), ce!(rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs / rhs).into())),
            (lhs, rhs) => Div(lhs.into(), rhs.into()),
        },
        Mul(lhs, rhs) => match (ce!(lhs), ce!(rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs * rhs).into())),
            (lhs, rhs) => Mul(lhs.into(), rhs.into()),
        },
        Mod(lhs, rhs) => match (ce!(lhs), ce!(rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs % rhs).into())),
            (lhs, rhs) => Mod(lhs.into(), rhs.into()),
        },
        Call { fun, args } => Call {
            fun: fun.clone(),
            args: args.into_iter().map(|expr| ce!(expr)).collect(),
        },
        Assignment { .. } => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use anathema_values::testing::{dot, ident, index, inum, list, map, unum};

    use super::*;
    use crate::testing::TestScope;

    #[test]
    fn test_eval_dot() {
        // a.b = 1
        // resolve a.b
        let mut test_scope = TestScope::new();
        test_scope.local("a", map([("b", inum(1))]));
        test_scope.exec();
        let vars = test_scope.vars;
        let expr = dot(ident("a"), ident("b"));
        let expr = eval_dot(&expr, &vars).unwrap();
        assert_eq!(expr, &Expression::Owned(1.into()));
    }

    #[test]
    fn test_eval_index() {
        // a.b = [1]
        // c = 0
        // resolve a.b[c]
        let mut test_scope = TestScope::new();
        test_scope.local("a", map([("b", list([inum(1)]))]));
        test_scope.local("c", unum(0));
        test_scope.exec();

        let vars = test_scope.vars;
        let expr = index(dot(ident("a"), ident("b")), ident("c"));
        let expr = eval_dot(&expr, &vars).unwrap();
        assert_eq!(expr, &Expression::Owned(1.into()));
    }

    #[test]
    fn assign() {
        // a.b = [1]
        // c = 0
        // resolve a.b[c]
        let mut test_scope = TestScope::new();
        test_scope.local("a", map([("b", list([inum(1)]))]));
        test_scope.local("c", unum(0));
        test_scope.exec();

        let vars = test_scope.vars;
        let expr = index(dot(ident("a"), ident("b")), ident("c"));
        let expr = eval_dot(&expr, &vars).unwrap();
        assert_eq!(expr, &Expression::Owned(1.into()));
    }
}
