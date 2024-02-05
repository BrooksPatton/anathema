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
fn eval_dot(expr: &Expression, vars: &Variables) -> Option<Expression> {
    match expr {
        Expression::Ident(ident) => {
            let ident: &str = &*ident;
            vars.fetch(ident)
        }
        Expression::Index(lhs, index) => match eval_dot(lhs, vars)? {
            Expression::List(list) => match &**index {
                Expression::Owned(Own::Num(num)) => list.get(num.to_usize()).cloned(),
                _ => Some(Expression::Index(
                    Expression::List(list.clone()).into(),
                    const_eval(*index.clone(), vars).into(),
                )),
            },
            Expression::Map(map) => match &**index {
                Expression::Str(key) => map.get(&**key).cloned(),
                Expression::Ident(key) => match vars.fetch(key)? {
                    Expression::Str(key) => map.get(&*key).cloned(),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        Expression::Dot(lhs, rhs) => match eval_dot(&**lhs, vars)? {
            Expression::Map(map) => match &**rhs {
                Expression::Ident(key) => map.get(&**key).cloned(),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn const_eval(expr: Expression, vars: &Variables) -> Expression {
    use Expression::*;

    macro_rules! ce {
        ($e:expr) => {
            const_eval($e, vars).into()
        };
    }

    match expr {
        expr @ (Owned(_) | Str(_)) => expr,
        Not(expr) => Not(ce!(*expr)),
        Negative(expr) => Negative(ce!(*expr)),
        And(lhs, rhs) => And(ce!(*lhs), ce!(*rhs)),
        Or(lhs, rhs) => Or(ce!(*lhs), ce!(*rhs)),
        Equality(lhs, rhs) => Equality(ce!(*lhs), ce!(*rhs)),
        Greater(lhs, rhs) => Greater(ce!(*lhs), ce!(*rhs)),
        GreaterEqual(lhs, rhs) => GreaterEqual(ce!(*lhs), ce!(*rhs)),
        Less(lhs, rhs) => Less(ce!(*lhs), ce!(*rhs)),
        LessEqual(lhs, rhs) => LessEqual(ce!(*lhs), ce!(*rhs)),

        Ident(ref ident) => vars.fetch(&*ident).unwrap_or(expr),
        Dot(..) => eval_dot(&expr, vars).unwrap_or(expr),
        Index(..) => eval_dot(&expr, vars).unwrap_or(expr),

        List(list) => {
            let list = list.into_iter().cloned().map(|expr| ce!(expr)).collect();
            List(list)
        }
        Map(map) => {
            let hm = HashMap::from_iter(map.iter().map(|(k, v)| (k.clone(), ce!(v.clone()))));
            Map(hm.into())
        }
        Add(lhs, rhs) => match (ce!(*lhs), ce!(*rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs + rhs).into())),
            (lhs, rhs) => Add(lhs.into(), rhs.into()),
        },
        Sub(lhs, rhs) => match (ce!(*lhs), ce!(*rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs - rhs).into())),
            (lhs, rhs) => Sub(lhs.into(), rhs.into()),
        },
        Div(lhs, rhs) => match (ce!(*lhs), ce!(*rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs / rhs).into())),
            (lhs, rhs) => Div(lhs.into(), rhs.into()),
        },
        Mul(lhs, rhs) => match (ce!(*lhs), ce!(*rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs * rhs).into())),
            (lhs, rhs) => Mul(lhs.into(), rhs.into()),
        },
        Mod(lhs, rhs) => match (ce!(*lhs), ce!(*rhs)) {
            (Owned(Own::Num(lhs)), Owned(Own::Num(rhs))) => Owned(Own::Num((lhs % rhs).into())),
            (lhs, rhs) => Mod(lhs.into(), rhs.into()),
        },
        Call { fun, args } => Call {
            fun: fun.clone(),
            args: args.into_iter().map(|expr| ce!(expr.clone())).collect(),
        },
    }
}

#[cfg(test)]
mod test {
    use anathema_values::testing::{add, dot, ident, index, inum, list, map, strlit, unum};

    use super::*;
    use crate::testing::TestScope;

    #[test]
    fn declare_twice_const_folding() {
        let mut test_scope = TestScope::new();
        test_scope.local("a", inum(1));
        test_scope.local("b", ident("a"));
        test_scope.exec();

        let a = test_scope.vars.by_value_ref(0.into());
        let b = test_scope.vars.by_value_ref(1.into());
        let expected: Expression = 1.into();
        assert_eq!(expected, a);
        assert_eq!(expected, b);
    }

    #[test]
    fn eval_map() {
        // a.b = 1
        // resolve a.b
        let mut test_scope = TestScope::new();
        test_scope.local("a", map([("b", inum(1))]));
        test_scope.exec();
        let vars = test_scope.vars;
        let expr = dot(ident("a"), ident("b"));
        let expr = eval_dot(&expr, &vars).unwrap();
        assert_eq!(expr, Expression::Owned(1.into()));
    }

    #[test]
    fn eval_index() {
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
        assert_eq!(expr, Expression::Owned(1.into()));
    }

    #[test]
    fn list_const_eval() {
        let mut test_scope = TestScope::new();
        test_scope.local("a", list([strlit("red"), strlit("blue")]));
        test_scope.local("b", index(ident("a"), unum(1)));
        let _ = test_scope.exec();
        let actual = test_scope.vars.fetch("b").unwrap();
        let expected = *strlit("blue");
        assert_eq!(expected, actual);
    }

    #[test]
    fn list_dyn_eval() {
        let mut test_scope = TestScope::new();
        test_scope.local("a", list([strlit("red"), strlit("blue")]));
        test_scope.local("b", unum(1));
        test_scope.exec();

        let expr = index(ident("a"), add(ident("some_state"), ident("b")));
        let actual = eval_dot(&expr, &test_scope.vars).unwrap();
        let expected = *index(
            list([strlit("red"), strlit("blue")]),
            add(ident("some_state"), unum(1)),
        );
        assert_eq!(expected, actual);
    }
}
