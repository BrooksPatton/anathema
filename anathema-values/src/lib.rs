use std::cell::RefCell;

pub use anathema_value_derive::State;

pub use self::collection::Collection;
pub use self::constants::{Constants, StringId, ValueId, ViewId, ViewIds};
pub use self::id::{NextNodeId, NodeId};
pub use self::list::List;
pub use self::map::Map;
pub use self::path::Path;
pub use self::scope::{Context, OwnedScopeValues, Scope, ScopeValue, Scopes};
pub use self::slab::Slab;
pub use self::state::{Change, State, StateValue};
pub use self::value::{ExpressionMap, Expressions, Num, Owned, ValueRef};
pub use self::value_expr::{Deferred, Expression, Immediate, Resolver, Visibility};
pub use self::variables::{Variable, Variables};

pub mod hashmap;
mod path;
pub mod signals;

mod collection;
mod constants;
mod id;
mod list;
mod map;
mod scope;
mod slab;
pub mod state;
mod value;
mod value_expr;
mod variables;

// -----------------------------------------------------------------------------
//   - Macro requirements -
// -----------------------------------------------------------------------------
#[allow(unused_extern_crates)]
extern crate self as anathema;
#[allow(unused_imports)]
pub use crate as values;

pub type Attributes = hashmap::HashMap<String, Expression>;

thread_local! {
    static DIRTY_NODES: RefCell<Vec<(NodeId, Change)>> = Default::default();
}

pub fn drain_dirty_nodes() -> Vec<(NodeId, Change)> {
    DIRTY_NODES.with(|nodes| nodes.borrow_mut().drain(..).collect())
}

#[cfg(any(feature = "testing", test))]
pub mod testing;

#[derive(Debug, Default)]
pub enum Value<T> {
    Dyn {
        inner: Option<T>,
        expr: Expression,
    },
    Static(T),
    #[default]
    Empty,
}

impl<T> Value<T>
where
    T: DynValue,
{
    pub fn resolve(&mut self, context: &Context<'_, '_>, node_id: &NodeId) {
        T::resolve(self, context, node_id);
    }
}

impl<T> Value<T> {
    pub fn value_ref(&self) -> Option<&T> {
        match self {
            Self::Static(val) => Some(val),
            Self::Dyn { inner, .. } => inner.as_ref(),
            _ => None,
        }
    }

    pub fn take(&mut self) -> Self {
        std::mem::take(self)
    }
}

impl<T: Copy> Value<T> {
    pub fn value(&self) -> Option<T> {
        match self {
            Self::Static(val) => Some(*val),
            &Self::Dyn { inner, .. } => inner,
            _ => None,
        }
    }

    pub fn value_or(&self, default: T) -> T {
        match self {
            Self::Static(val) => Some(*val),
            &Self::Dyn { inner, .. } => inner,
            _ => None,
        }
        .unwrap_or(default)
    }

    pub fn value_or_else<F>(&self, default: F) -> T
    where
        F: Fn() -> T,
    {
        match self {
            Self::Static(val) => Some(*val),
            &Self::Dyn { inner, .. } => inner,
            _ => None,
        }
        .unwrap_or_else(default)
    }
}

impl<T: Default + Copy> Value<T> {
    pub fn value_or_default(&self) -> T {
        match self {
            Self::Static(val) => Some(*val),
            &Self::Dyn { inner, .. } => inner,
            _ => None,
        }
        .unwrap_or_else(T::default)
    }
}

impl Value<bool> {
    pub fn is_true(&self) -> bool {
        match self {
            Self::Dyn { inner, .. } => inner.unwrap_or(false),
            Self::Static(b) => *b,
            Self::Empty => false,
        }
    }
}

impl Value<String> {
    pub fn str(&self) -> &str {
        static EMPTY: &str = "";

        match self {
            Self::Static(s) => s,
            Self::Dyn { inner: Some(s), .. } => s,
            Self::Dyn { inner: None, .. } => EMPTY,
            Self::Empty => EMPTY,
        }
    }
}

pub trait DynValue {
    fn init_value(context: &Context<'_, '_>, node_id: &NodeId, expr: &Expression) -> Value<Self>
    where
        Self: Sized;

    fn resolve(value: &mut Value<Self>, context: &Context<'_, '_>, node_id: &NodeId)
    where
        Self: Sized;
}

impl DynValue for String {
    fn init_value(context: &Context<'_, '_>, node_id: &NodeId, expr: &Expression) -> Value<Self> {
        let mut resolver = Immediate::new(context.lookup(), node_id);
        let inner = expr.eval_string(&mut resolver);

        match resolver.is_deferred() {
            true => Value::Dyn {
                inner,
                expr: expr.clone(),
            },
            false => match inner {
                Some(val) => Value::Static(val),
                None => Value::Empty,
            },
        }
    }

    fn resolve(value: &mut Value<Self>, context: &Context<'_, '_>, node_id: &NodeId) {
        if let Value::Dyn { inner, expr } = value {
            let mut resolver = Immediate::new(context.lookup(), node_id);
            *inner = expr.eval_string(&mut resolver)
        }
    }
}

#[macro_export]
macro_rules! impl_dyn_value {
    ($t:ty) => {
        impl DynValue for $t {
            fn init_value(
                context: &Context<'_, '_>,
                node_id: &NodeId,
                expr: &Expression,
            ) -> Value<Self> {
                let mut resolver = Immediate::new(context.lookup(), node_id);
                let inner = expr.eval_value(&mut resolver).try_into().ok();

                match resolver.is_deferred() {
                    true => Value::Dyn {
                        inner,
                        expr: expr.clone(),
                    },
                    false => match inner {
                        None => Value::Empty,
                        Some(val) => Value::Static(val),
                    },
                }
            }

            fn resolve(value: &mut Value<Self>, context: &Context<'_, '_>, node_id: &NodeId) {
                match value {
                    Value::Dyn { inner, expr } => {
                        let mut resolver = Immediate::new(context.lookup(), node_id);
                        *inner = expr.eval_value(&mut resolver).try_into().ok()
                    }
                    _ => {}
                }
            }
        }
    };
}

impl DynValue for bool {
    fn init_value(context: &Context<'_, '_>, node_id: &NodeId, expr: &Expression) -> Value<Self> {
        let mut resolver = Immediate::new(context.lookup(), node_id);
        let val = expr.eval_value(&mut resolver);
        match resolver.is_deferred() {
            true => Value::Dyn {
                inner: Some(val.is_true()),
                expr: expr.clone(),
            },
            false => match val {
                ValueRef::Empty => Value::Empty,
                val => Value::Static(val.is_true()),
            },
        }
    }

    fn resolve(value: &mut Value<Self>, context: &Context<'_, '_>, node_id: &NodeId) {
        if let Value::Dyn { inner, expr } = value {
            let mut resolver = Immediate::new(context.lookup(), node_id);
            *inner = Some(expr.eval_value(&mut resolver).is_true());
        }
    }
}

impl DynValue for anathema_render::Color {
    fn init_value(context: &Context<'_, '_>, node_id: &NodeId, expr: &Expression) -> Value<Self> {
        let mut resolver = Immediate::new(context.lookup(), node_id);
        let inner = match expr.eval_value(&mut resolver) {
            ValueRef::Str(col) => anathema_render::Color::try_from(col).ok(),
            val => val.try_into().ok(),
        };

        match resolver.is_deferred() {
            true => Value::Dyn {
                inner,
                expr: expr.clone(),
            },
            false => match inner {
                Some(val) => Value::Static(val),
                None => Value::Empty,
            },
        }
    }

    fn resolve(value: &mut Value<Self>, context: &Context<'_, '_>, node_id: &NodeId) {
        if let Value::Dyn { inner, expr } = value {
            let mut resolver = Immediate::new(context.lookup(), node_id);
            *inner = expr.eval_value(&mut resolver).try_into().ok()
        }
    }
}

// impl_dyn_value!(anathema_render::Color);

impl_dyn_value!(usize);
impl_dyn_value!(u64);
impl_dyn_value!(u32);
impl_dyn_value!(u16);
impl_dyn_value!(u8);

impl_dyn_value!(isize);
impl_dyn_value!(i64);
impl_dyn_value!(i32);
impl_dyn_value!(i16);
impl_dyn_value!(i8);

impl_dyn_value!(f64);
impl_dyn_value!(f32);

impl_dyn_value!(char);
