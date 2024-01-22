use crate::map::Map;
use crate::{Context, Immediate, List, NodeId, Owned, StateValue, ExpressionBanana, ValueRef};

#[derive(Debug, crate::State)]
pub struct Inner {
    pub name: StateValue<String>,
    pub names: List<String>,
}

impl Inner {
    pub fn new() -> Self {
        Self {
            name: StateValue::new("Fiddle McStick".into()),
            names: List::new(vec!["arthur".to_string(), "bobby".into()]),
        }
    }
}

#[derive(Debug, crate::State)]
pub struct TestState {
    pub name: StateValue<String>,
    pub counter: StateValue<usize>,
    pub inner: Inner,
    pub generic_map: Map<Map<usize>>,
    pub generic_list: List<usize>,
    pub nested_list: List<List<usize>>,
    pub debug: StateValue<bool>,
}

impl TestState {
    pub fn new() -> Self {
        Self {
            name: StateValue::new("Dirk Gently".to_string()),
            counter: StateValue::new(3),
            inner: Inner::new(),
            generic_map: Map::new([("inner", Map::new([("first", 1), ("second", 2)]))]),
            generic_list: List::new(vec![1, 2, 3]),
            nested_list: List::new(vec![List::new(vec![1, 2, 3])]),
            debug: StateValue::new(false),
        }
    }
}

// -----------------------------------------------------------------------------
//   - Extend value expression -
// -----------------------------------------------------------------------------
#[derive(Debug)]
pub struct TestExpression<'expr, T> {
    pub state: Map<T>,
    pub expr: &'expr ExpressionBanana,
    node_id: NodeId,
}

impl<'expr, T: std::fmt::Debug> TestExpression<'expr, T>
where
    for<'a> &'a T: Into<ValueRef<'a>>,
{
    pub fn cmp(&self, other: ValueRef<'_>) {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        let val = self.expr.eval(&mut resolver);
        assert_eq!(val, other)
    }

    pub fn expect_string(&self, cmp: &str) {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        let s = self.expr.eval_string(&mut resolver).unwrap();
        assert_eq!(s, cmp);
    }

    pub fn eval_bool(&self, b: bool) -> bool {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        self.expr.eval(&mut resolver).is_true() == b
    }

    pub fn expect_owned(&self, expected: impl Into<Owned>) {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        let val = self.expr.eval(&mut resolver);
        let ValueRef::Owned(owned) = val else {
            panic!("not an owned value")
        };
        assert_eq!(owned, expected.into())
    }
}

impl ExpressionBanana {
    pub fn with_data<T, K: Into<String>>(
        &self,
        inner: impl IntoIterator<Item = (K, T)>,
    ) -> TestExpression<'_, T> {
        TestExpression {
            state: Map::new(inner),
            expr: self,
            node_id: 0.into(),
        }
    }

    pub fn test(&self) -> TestExpression<'_, usize> {
        TestExpression {
            state: Map::empty(),
            expr: self,
            node_id: 0.into(),
        }
    }
}

// -----------------------------------------------------------------------------
//   - Paths -
// -----------------------------------------------------------------------------
pub fn ident(p: &str) -> Box<ExpressionBanana> {
    ExpressionBanana::Ident(p.into()).into()
}

pub fn index(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Index(lhs, rhs).into()
}

pub fn dot(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Dot(lhs, rhs).into()
}

// -----------------------------------------------------------------------------
//   - Maths -
// -----------------------------------------------------------------------------
pub fn mul(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Mul(lhs, rhs).into()
}

pub fn div(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Div(lhs, rhs).into()
}

pub fn modulo(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Mod(lhs, rhs).into()
}

pub fn sub(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Sub(lhs, rhs).into()
}

pub fn add(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Add(lhs, rhs).into()
}

pub fn greater_than(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Greater(lhs, rhs).into()
}

pub fn greater_than_equal(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::GreaterEqual(lhs, rhs).into()
}

pub fn less_than(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Less(lhs, rhs).into()
}

pub fn less_than_equal(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::LessEqual(lhs, rhs).into()
}

// -----------------------------------------------------------------------------
//   - Values -
// -----------------------------------------------------------------------------
pub fn unum(int: u64) -> Box<ExpressionBanana> {
    ExpressionBanana::Owned(Owned::from(int)).into()
}

pub fn inum(int: i64) -> Box<ExpressionBanana> {
    ExpressionBanana::Owned(Owned::from(int)).into()
}

pub fn boolean(b: bool) -> Box<ExpressionBanana> {
    ExpressionBanana::Owned(Owned::from(b)).into()
}

pub fn strlit(lit: &str) -> Box<ExpressionBanana> {
    ExpressionBanana::String(lit.into()).into()
}

// -----------------------------------------------------------------------------
//   - List -
// -----------------------------------------------------------------------------
pub fn list<E: Into<ExpressionBanana>>(input: impl IntoIterator<Item = E>) -> Box<ExpressionBanana> {
    let vec = input.into_iter().map(|val| val.into()).collect::<Vec<_>>();
    ExpressionBanana::List(vec.into()).into()
}

// -----------------------------------------------------------------------------
//   - Op -
// -----------------------------------------------------------------------------
pub fn neg(expr: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Negative(expr).into()
}

pub fn not(expr: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Not(expr).into()
}

pub fn eq(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Equality(lhs, rhs).into()
}

pub fn and(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::And(lhs, rhs).into()
}

pub fn or(lhs: Box<ExpressionBanana>, rhs: Box<ExpressionBanana>) -> Box<ExpressionBanana> {
    ExpressionBanana::Or(lhs, rhs).into()
}
