use crate::hashmap::HashMap;
use crate::map::Map;
use crate::{Context, Expression, Immediate, List, NodeId, Owned, StateValue, ValueRef};

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
    pub expr: &'expr Expression,
    node_id: NodeId,
}

impl<'expr, T: std::fmt::Debug> TestExpression<'expr, T>
where
    for<'a> &'a T: Into<ValueRef<'a>>,
{
    pub fn cmp(&self, other: ValueRef<'_>) {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        let val = self.expr.resolve_value(&mut resolver);
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
        self.expr.resolve_value(&mut resolver).is_true() == b
    }

    pub fn expect_owned(&self, expected: impl Into<Owned>) {
        let context = Context::root(&self.state);
        let mut resolver = Immediate::new(context.lookup(), &self.node_id);
        let val = self.expr.resolve_value(&mut resolver);
        let ValueRef::Owned(owned) = val else {
            panic!("not an owned value")
        };
        assert_eq!(owned, expected.into())
    }
}

impl Expression {
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
pub fn ident(p: &str) -> Box<Expression> {
    Expression::Ident(p.into()).into()
}

pub fn index(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Index(lhs, rhs).into()
}

pub fn dot(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Dot(lhs, rhs).into()
}

// -----------------------------------------------------------------------------
//   - Maths -
// -----------------------------------------------------------------------------
pub fn mul(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Mul(lhs, rhs).into()
}

pub fn div(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Div(lhs, rhs).into()
}

pub fn modulo(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Mod(lhs, rhs).into()
}

pub fn sub(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Sub(lhs, rhs).into()
}

pub fn add(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Add(lhs, rhs).into()
}

pub fn greater_than(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Greater(lhs, rhs).into()
}

pub fn greater_than_equal(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::GreaterEqual(lhs, rhs).into()
}

pub fn less_than(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Less(lhs, rhs).into()
}

pub fn less_than_equal(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::LessEqual(lhs, rhs).into()
}

// -----------------------------------------------------------------------------
//   - Values -
// -----------------------------------------------------------------------------
pub fn unum(int: u64) -> Box<Expression> {
    Expression::Static(Owned::from(int)).into()
}

pub fn inum(int: i64) -> Box<Expression> {
    Expression::Static(Owned::from(int)).into()
}

pub fn boolean(b: bool) -> Box<Expression> {
    Expression::Static(Owned::from(b)).into()
}

pub fn strlit(lit: &str) -> Box<Expression> {
    Expression::Str(lit.into()).into()
}

// -----------------------------------------------------------------------------
//   - List and map -
// -----------------------------------------------------------------------------
pub fn list<E: Into<Expression>>(input: impl IntoIterator<Item = E>) -> Box<Expression> {
    let vec = input.into_iter().map(|val| val.into()).collect::<Vec<_>>();
    Expression::List(vec.into()).into()
}

pub fn map<E: Into<Expression>>(
    input: impl IntoIterator<Item = (&'static str, E)>,
) -> Box<Expression> {
    let input = input.into_iter().map(|(k, v)| (k.to_string(), v.into()));
    let hm: HashMap<String, Expression> = HashMap::from_iter(input);
    Expression::Map(hm.into()).into()
}

// -----------------------------------------------------------------------------
//   - Op -
// -----------------------------------------------------------------------------
pub fn neg(expr: Box<Expression>) -> Box<Expression> {
    Expression::Negative(expr).into()
}

pub fn not(expr: Box<Expression>) -> Box<Expression> {
    Expression::Not(expr).into()
}

pub fn eq(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Equality(lhs, rhs).into()
}

pub fn and(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::And(lhs, rhs).into()
}

pub fn or(lhs: Box<Expression>, rhs: Box<Expression>) -> Box<Expression> {
    Expression::Or(lhs, rhs).into()
}
