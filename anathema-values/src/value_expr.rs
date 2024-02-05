use std::fmt::Display;
use std::rc::Rc;

use crate::hashmap::HashMap;
use crate::scope::ContextRef;
use crate::value::{ExpressionMap, Expressions};
use crate::{Collection, NodeId, Owned, Path, ScopeValue, State, ValueRef};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    Global,
    Local,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Global => write!(f, "global"),
            Self::Local => write!(f, "local"),
        }
    }
}

// -----------------------------------------------------------------------------
//   - Value resolver trait -
// -----------------------------------------------------------------------------
pub trait Resolver<'expr> {
    fn resolve(&mut self, path: Path<'_>) -> ValueRef<'expr>;

    fn resolve_list(&mut self, list: &'expr dyn Collection, index: usize) -> ValueRef<'expr>;

    fn resolve_map(&mut self, map: &'expr dyn State, key: &str) -> ValueRef<'expr>;
}

// -----------------------------------------------------------------------------
//   - Deferred -
// -----------------------------------------------------------------------------
/// Only resolve up until a deferred path.
/// This means `ValueExpr::Deferred` will not be resolved, and instead returned.
pub struct Deferred<'a, 'expr> {
    context: ContextRef<'a, 'expr>,
}

impl<'a, 'expr> Deferred<'a, 'expr> {
    pub fn new(context: ContextRef<'a, 'expr>) -> Self {
        Self { context }
    }
}

impl<'a, 'expr> Resolver<'expr> for Deferred<'a, 'expr> {
    fn resolve(&mut self, path: Path<'_>) -> ValueRef<'expr> {
        match self.context.scopes(path) {
            None => {
                if let Some(context) = self.context.pop() {
                    let mut resolver = Self::new(context);
                    resolver.resolve(path)
                } else {
                    ValueRef::Deferred
                }
            }
            Some(ScopeValue::Value(value)) => value,
            Some(ScopeValue::Deferred(..)) => ValueRef::Deferred,
            Some(ScopeValue::DeferredList(..)) => ValueRef::Deferred,
        }
    }

    fn resolve_list(&mut self, _: &dyn Collection, _: usize) -> ValueRef<'expr> {
        ValueRef::Deferred
    }

    fn resolve_map(&mut self, _: &dyn State, _: &str) -> ValueRef<'expr> {
        ValueRef::Deferred
    }
}

// -----------------------------------------------------------------------------
//   - Resolver -
//   This should never return a deferred value, instead
//   it should resolve any deferred value before returning
//
//   The immediate resolver is the only resolver that will
//   access the state, therefore no other resolver needs a NodeId
// -----------------------------------------------------------------------------
/// Resolve the expression, including deferred values.
pub struct Immediate<'frame> {
    context: ContextRef<'frame, 'frame>,
    node_id: &'frame NodeId,
    is_deferred: bool,
}

impl<'frame> Immediate<'frame> {
    pub fn new(context: ContextRef<'frame, 'frame>, node_id: &'frame NodeId) -> Self {
        Self {
            context,
            node_id,
            is_deferred: false,
        }
    }
}

impl Immediate<'_> {
    pub fn is_deferred(&self) -> bool {
        self.is_deferred
    }
}

impl<'frame> Resolver<'frame> for Immediate<'frame> {
    fn resolve(&mut self, path: Path<'_>) -> ValueRef<'frame> {
        // 1. state
        // 2. scope -> state, scope, [parent]---+
        // 3. parent                            |
        //    |                                 |
        // +--+---------------------------------+
        // |  __________________________________
        // | / Once lookup occurs in the parent |
        // |/  it should not traverse back up   |
        //  \  to the most recent state         |
        //   `----------------------------------'

        // loop:
        //     context.get_state()
        //     context.get_scope()
        //     resolver = new resolver with context.pop();
        //     if resolver.is_deferred {
        //         self.is_deferred = true;
        //     }

        match self.context.state(path, self.node_id) {
            ValueRef::Empty => match self.context.scopes(path) {
                None => {
                    if let Some(context) = self.context.pop() {
                        let mut resolver = Self::new(context, self.node_id);
                        let val = resolver.resolve(path);
                        if resolver.is_deferred {
                            self.is_deferred = true;
                        }
                        val
                    } else {
                        ValueRef::Empty
                    }
                }
                Some(ScopeValue::Value(val)) => val,
                Some(ScopeValue::Deferred(expr)) => {
                    self.is_deferred = true;
                    expr.eval_value(self)
                }
                Some(ScopeValue::DeferredList(index, expr)) => {
                    self.is_deferred = true;
                    match expr.eval_value(self) {
                        ValueRef::Expressions(expressions) => expressions
                            .get(index)
                            .expect("Index bounds check in loop expression")
                            .eval_value(self),
                        ValueRef::List(list) => {
                            let path = index.into();
                            list.state_get(path, self.node_id)
                        }
                        _ => ValueRef::Empty,
                    }
                }
            },
            val => {
                self.is_deferred = true;
                val
            }
        }
    }

    fn resolve_list(&mut self, list: &'frame dyn Collection, index: usize) -> ValueRef<'frame> {
        let path = index.into();
        self.is_deferred = true;
        list.state_get(path, self.node_id)
    }

    fn resolve_map(&mut self, map: &'frame dyn State, key: &str) -> ValueRef<'frame> {
        let path = key.into();
        self.is_deferred = true;
        map.state_get(path, self.node_id)
    }
}

// -----------------------------------------------------------------------------
//   - Value expressoin -
// -----------------------------------------------------------------------------
// TODO
// Change Box<T> to Rc<T> once there are some
// benchmarks in place to justify this change
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Owned(Owned),
    Str(Rc<str>),

    Not(Box<Expression>),
    Negative(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Equality(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>),
    GreaterEqual(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),

    Ident(Rc<str>),
    Dot(Box<Expression>, Box<Expression>),
    Index(Box<Expression>, Box<Expression>),

    // List and Map are both Rc'd as expressions are
    // cloned for `Value<T>` and a few other places.
    List(Rc<[Expression]>),
    Map(Rc<HashMap<String, Expression>>),

    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),

    Call {
        fun: Box<Expression>,
        args: Box<[Expression]>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Owned(val) => write!(f, "{val}"),
            Self::Str(val) => write!(f, "{val}"),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Index(lhs, idx) => write!(f, "{lhs}[{idx}]"),
            Self::Dot(lhs, rhs) => write!(f, "{lhs}.{rhs}"),
            Self::Not(expr) => write!(f, "!{expr}"),
            Self::Negative(expr) => write!(f, "-{expr}"),
            Self::Add(lhs, rhs) => write!(f, "{lhs} + {rhs}"),
            Self::Sub(lhs, rhs) => write!(f, "{lhs} - {rhs}"),
            Self::Mul(lhs, rhs) => write!(f, "{lhs} * {rhs}"),
            Self::Div(lhs, rhs) => write!(f, "{lhs} / {rhs}"),
            Self::Mod(lhs, rhs) => write!(f, "{lhs} % {rhs}"),
            Self::List(list) => {
                write!(
                    f,
                    "[{}]",
                    list.iter()
                        .map(|val| val.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Map(map) => {
                write!(
                    f,
                    "{{{}}}",
                    map.iter()
                        .map(|(key, val)| format!("{key}: {val}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::And(lhs, rhs) => write!(f, "{lhs} && {rhs}"),
            Self::Or(lhs, rhs) => write!(f, "{lhs} || {rhs}"),
            Self::Equality(lhs, rhs) => write!(f, "{lhs} == {rhs}"),
            Self::Greater(lhs, rhs) => write!(f, "{lhs} > {rhs}"),
            Self::GreaterEqual(lhs, rhs) => write!(f, "{lhs} >= {rhs}"),
            Self::Less(lhs, rhs) => write!(f, "{lhs} < {rhs}"),
            Self::LessEqual(lhs, rhs) => write!(f, "{lhs} <= {rhs}"),
            Self::Call { fun, args } => {
                write!(
                    f,
                    "{fun}({})",
                    args.iter()
                        .map(|val| val.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

macro_rules! eval_num {
    ($e:expr, $resolver:expr) => {
        match $e.eval_value($resolver) {
            ValueRef::Owned(Owned::Num(num)) => num,
            ValueRef::Deferred => return ValueRef::Deferred,
            _ => return ValueRef::Empty,
        }
    };
}

impl Expression {
    pub fn eval_string<'expr>(&'expr self, resolver: &mut impl Resolver<'expr>) -> Option<String> {
        match self.eval_value(resolver) {
            ValueRef::Str(s) => Some(s.into()),
            ValueRef::Owned(s) => Some(s.to_string()),
            ValueRef::Expressions(Expressions(list)) => {
                let mut s = String::new();
                for expr in list {
                    let res = expr.eval_string(resolver);
                    if let Some(res) = res {
                        s.push_str(&res);
                    }
                }
                Some(s)
            }
            ValueRef::Map(_) => Some("<map>".to_string()),
            ValueRef::List(_) => Some("<list>".to_string()),
            ValueRef::ExpressionMap(_) => Some("<expr map>".to_string()),
            ValueRef::Deferred => None,
            ValueRef::Empty => None,
        }
    }

    pub fn eval_vec<'expr>(
        &'expr self,
        resolver: &mut impl Resolver<'expr>,
    ) -> Option<Vec<ValueRef<'_>>> {
        match self.eval_value(resolver) {
            ValueRef::Expressions(Expressions(list)) => {
                let mut v = Vec::with_capacity(list.len());
                for expr in list {
                    let res = expr.eval_value(resolver);
                    v.push(res);
                }
                Some(v)
            }
            _ => None,
        }
    }

    // Even though the lifetime is named `'expr`, the value isn't necessarily tied to an expression.
    //
    // Static values originate from expressions and will have the aforementioned lifetime,
    // however a value could also stem from a state (by resolving a deferred value).
    // A value that originates from `State` can only live for the duration of the layout phase.
    pub fn eval_value<'expr>(&'expr self, resolver: &mut impl Resolver<'expr>) -> ValueRef<'expr> {
        match self {
            Self::Owned(value) => ValueRef::Owned(*value),
            Self::Str(value) => ValueRef::Str(value),

            // -----------------------------------------------------------------------------
            //   - Maths -
            // -----------------------------------------------------------------------------
            op @ (Self::Add(lhs, rhs)
            | Self::Sub(lhs, rhs)
            | Self::Mul(lhs, rhs)
            | Self::Mod(lhs, rhs)
            | Self::Div(lhs, rhs)
            | Self::Greater(lhs, rhs)
            | Self::GreaterEqual(lhs, rhs)
            | Self::Less(lhs, rhs)
            | Self::LessEqual(lhs, rhs)) => {
                let lhs = eval_num!(lhs, resolver);
                let rhs = eval_num!(rhs, resolver);

                match op {
                    Self::Add(..) => ValueRef::Owned(Owned::Num(lhs + rhs)),
                    Self::Sub(..) => ValueRef::Owned(Owned::Num(lhs - rhs)),
                    Self::Mul(..) => ValueRef::Owned(Owned::Num(lhs * rhs)),
                    Self::Mod(..) => ValueRef::Owned(Owned::Num(lhs % rhs)),
                    Self::Div(..) if !rhs.is_zero() => ValueRef::Owned(Owned::Num(lhs / rhs)),
                    Self::Div(..) => ValueRef::Empty,
                    Self::Greater(..) => ValueRef::Owned(Owned::Bool(lhs.to_f64() > rhs.to_f64())),
                    Self::GreaterEqual(..) => {
                        ValueRef::Owned(Owned::Bool(lhs.to_f64() >= rhs.to_f64()))
                    }
                    Self::Less(..) => ValueRef::Owned(Owned::Bool(lhs.to_f64() < rhs.to_f64())),
                    Self::LessEqual(..) => {
                        ValueRef::Owned(Owned::Bool(lhs.to_f64() <= rhs.to_f64()))
                    }
                    _ => unreachable!(),
                }
            }

            Self::Negative(expr) => {
                let num = eval_num!(expr, resolver);
                ValueRef::Owned(Owned::Num(num.to_negative()))
            }

            // -----------------------------------------------------------------------------
            //   - Conditions -
            // -----------------------------------------------------------------------------
            Self::Not(expr) => {
                let b = expr.eval_value(resolver).is_true();
                ValueRef::Owned((!b).into())
            }
            Self::Equality(lhs, rhs) => {
                let lhs = lhs.eval_value(resolver);
                let rhs = rhs.eval_value(resolver);
                ValueRef::Owned((lhs == rhs).into())
            }
            Self::Or(lhs, rhs) => {
                let lhs = lhs.eval_value(resolver);
                let rhs = rhs.eval_value(resolver);
                ValueRef::Owned((lhs.is_true() || rhs.is_true()).into())
            }
            Self::And(lhs, rhs) => {
                let lhs = lhs.eval_value(resolver);
                let rhs = rhs.eval_value(resolver);
                ValueRef::Owned((lhs.is_true() && rhs.is_true()).into())
            }

            // -----------------------------------------------------------------------------
            //   - Paths -
            // -----------------------------------------------------------------------------
            Self::Ident(ident) => {
                let path = Path::from(&**ident);
                resolver.resolve(path)
            }
            Self::Index(lhs, index) => match lhs.eval_value(resolver) {
                ValueRef::Expressions(list) => {
                    let index = eval_num!(index, resolver).to_usize();
                    return list.0[index].eval_value(resolver);
                }
                ValueRef::ExpressionMap(map) => {
                    let key = index.eval_string(resolver).unwrap_or(String::new());
                    let expr = &map.0[&key];
                    expr.eval_value(resolver)
                }
                ValueRef::List(list) => {
                    let index = eval_num!(index, resolver).to_usize();
                    resolver.resolve_list(list, index)
                }
                ValueRef::Map(map) => {
                    let key = index.eval_string(resolver).unwrap_or(String::new());
                    resolver.resolve_map(map, &key)
                }
                deferred @ ValueRef::Deferred => deferred,
                _ => ValueRef::Empty,
            },
            Self::Dot(lhs, rhs) => match lhs.eval_value(resolver) {
                ValueRef::ExpressionMap(map) => {
                    let key = match &**rhs {
                        Expression::Ident(key) => key,
                        _ => return ValueRef::Empty,
                    };
                    return map.0[&**key].eval_value(resolver);
                }
                ValueRef::Map(map) => {
                    let key = match &**rhs {
                        Expression::Ident(key) => key,
                        _ => return ValueRef::Empty,
                    };
                    resolver.resolve_map(map, key)
                }
                deferred @ ValueRef::Deferred => deferred,
                _ => ValueRef::Empty,
            },

            // -----------------------------------------------------------------------------
            //   - Collection -
            // -----------------------------------------------------------------------------
            Self::List(list) => ValueRef::Expressions(Expressions::new(list)),
            Self::Map(map) => ValueRef::ExpressionMap(ExpressionMap::new(map)),

            // -----------------------------------------------------------------------------
            //   - Function call -
            // -----------------------------------------------------------------------------
            Self::Call { fun, args } => {
                let _fun_name = match &**fun {
                    Expression::Ident(name) => name,
                    _ => return ValueRef::Empty,
                };
                let _args = args.iter().map(|expr| expr.eval_value(resolver));

                panic!()
            }
        }
    }
}

impl From<Box<Expression>> for Expression {
    fn from(val: Box<Expression>) -> Self {
        *val
    }
}

impl<T> From<T> for Expression
where
    T: Into<Owned>,
{
    fn from(val: T) -> Self {
        Self::Owned(val.into())
    }
}

impl From<String> for Expression {
    fn from(val: String) -> Self {
        Self::Str(val.into())
    }
}

impl From<&str> for Expression {
    fn from(val: &str) -> Self {
        Self::Str(val.into())
    }
}

impl<const N: usize> From<[usize; N]> for Expression {
    fn from(value: [usize; N]) -> Self {
        let list = value.map(|n| Expression::from(n));
        Expression::List(list.into())
    }
}

#[cfg(test)]
mod test {
    use crate::map::Map;
    use crate::testing::{
        add, and, div, dot, eq, greater_than, greater_than_equal, ident, inum, less_than,
        less_than_equal, list, modulo, mul, neg, not, or, strlit, sub, unum,
    };

    #[test]
    fn add_dyn() {
        let expr = add(neg(inum(1)), neg(unum(2)));
        expr.with_data([("counter", 2usize)]).expect_owned(-3);
    }

    #[test]
    fn add_static() {
        let expr = add(neg(inum(1)), neg(unum(2)));
        expr.test().expect_owned(-3);
    }

    #[test]
    fn sub_static() {
        let expr = sub(unum(10), unum(2));
        expr.test().expect_owned(8u8);
    }

    #[test]
    fn mul_static() {
        let expr = mul(unum(10), unum(2));
        expr.test().expect_owned(20u8);
    }

    #[test]
    fn div_static() {
        let expr = div(unum(10), unum(2));
        expr.test().expect_owned(5u8);
    }

    #[test]
    fn mod_static() {
        let expr = modulo(unum(5), unum(3));
        expr.test().expect_owned(2u8);
    }

    #[test]
    fn greater_than_static() {
        let expr = greater_than(unum(5), unum(3));
        expr.test().expect_owned(true);
    }

    #[test]
    fn greater_than_equal_static() {
        let expr = greater_than_equal(unum(5), unum(3));
        expr.test().expect_owned(true);

        let expr = greater_than_equal(unum(3), unum(3));
        expr.test().expect_owned(true);
    }

    #[test]
    fn greater_than_equal_dynamic() {
        let expr = greater_than_equal(unum(5), ident("counter"));
        expr.with_data([("counter", 3)]).expect_owned(true);

        let expr = greater_than_equal(unum(5), ident("counter"));
        expr.with_data([("counter", 30)]).expect_owned(false);
    }

    #[test]
    fn less_than_static() {
        let expr = less_than(unum(2), unum(3));
        expr.test().expect_owned(true);
    }

    #[test]
    fn less_than_equal_static() {
        let expr = less_than_equal(unum(2), unum(3));
        expr.test().expect_owned(true);

        let expr = less_than_equal(unum(3), unum(3));
        expr.test().expect_owned(true);
    }

    #[test]
    fn bools() {
        // false
        let expr = ident("is_false");
        expr.with_data([("is_false", false)]).expect_owned(false);

        // not is false
        let expr = not(ident("is_false"));
        expr.with_data([("is_false", false)]).expect_owned(true);

        // equality
        let expr = eq(ident("one"), ident("one"));
        expr.with_data([("one", 1)]).eval_bool(true);

        // not equality
        let expr = not(eq(ident("one"), ident("two")));
        expr.with_data([("one", 1), ("two", 2)]).eval_bool(true);

        // or
        let expr = or(ident("one"), ident("two"));
        expr.with_data([("one", false), ("two", true)])
            .eval_bool(true);

        let expr = or(ident("one"), ident("two"));
        expr.with_data([("one", true), ("two", false)])
            .eval_bool(true);

        let expr = or(ident("one"), ident("two"));
        expr.with_data([("one", false), ("two", false)])
            .eval_bool(false);

        // and
        let expr = and(ident("one"), ident("two"));
        expr.with_data([("one", true), ("two", true)])
            .eval_bool(true);

        let expr = and(ident("one"), ident("two"));
        expr.with_data([("one", false), ("two", true)])
            .eval_bool(false);

        let expr = and(ident("one"), ident("two"));
        expr.with_data([("one", true), ("two", false)])
            .eval_bool(false);
    }

    #[test]
    fn path() {
        let test = dot(ident("inner"), ident("name"));
        test.with_data([("inner", Map::new([("name", "Fiddle McStick".to_string())]))])
            .expect_string("Fiddle McStick");
    }

    #[test]
    fn string() {
        let expr = list(vec![strlit("Mr. "), dot(ident("inner"), ident("name"))]);
        expr.with_data([("inner", Map::new([("name", "Fiddle McStick".to_string())]))])
            .expect_string("Mr. Fiddle McStick");
    }
}
