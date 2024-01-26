use anathema_values::hashmap::HashMap;
use anathema_values::{Expression, Owned as Own, Variables};

pub(crate) fn const_eval(expr: Expression, vars: &Variables) -> Expression {
    use Expression::*;

    macro_rules! ce {
        ($e:expr) => {
            const_eval($e, vars).into()
        };
    }

    match expr {
        expr @ (Owned(_) | Str(_) | Dot(_, _) | Index(_, _)) => expr,
        Not(expr) => Not(ce!(*expr)),
        Negative(expr) => Negative(ce!(*expr)),
        And(lhs, rhs) => And(ce!(*lhs), ce!(*rhs)),
        Or(lhs, rhs) => Or(ce!(*lhs), ce!(*rhs)),
        Equality(lhs, rhs) => Equality(ce!(*lhs), ce!(*rhs)),
        Greater(lhs, rhs) => Greater(ce!(*lhs), ce!(*rhs)),
        GreaterEqual(lhs, rhs) => GreaterEqual(ce!(*lhs), ce!(*rhs)),
        Less(lhs, rhs) => Less(ce!(*lhs), ce!(*rhs)),
        LessEqual(lhs, rhs) => LessEqual(ce!(*lhs), ce!(*rhs)),

        Ident(ident) => match vars.fetch(&ident) {
            Some(expr) => ce!(expr),
            None => Ident(ident),
        },
        List(list) => {
            let list = list.into_iter().map(|expr| ce!(expr.clone())).collect();
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
            fun,
            args: args.into_iter().map(|expr| ce!(expr.clone())).collect(),
        },
        Assignment { lhs, rhs } => todo!(),
    }
}
