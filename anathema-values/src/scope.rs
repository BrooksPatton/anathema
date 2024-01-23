use std::fmt::Debug;

use crate::hashmap::HashMap;
use crate::state::State;
use crate::{NodeId, Owned, Path, Expression, ValueRef};

/// Values owned by the nodes them selves.
#[derive(Debug)]
pub struct OwnedScopeValues<'e>(pub Vec<(&'e str, ScopeValue<'e>)>);

impl<'e> OwnedScopeValues<'e> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn insert(&mut self, key: &'e str, value: ScopeValue<'e>) {
        self.0.push((key, value));
    }

    pub fn head_tail(&self) -> Option<(&'e str, ScopeValue<'e>, &[(&'e str, ScopeValue<'e>)])> {
        self.0.first().copied().map(|(k, v)| (k, v, &self.0[1..]))
    }
}

// Scopes can only borrow values with the same lifetime as an expressions.
// Any scoped value that belongs to or contains state can only be scoped as a deferred expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScopeValue<'expr> {
    Value(ValueRef<'expr>),
    Deferred(&'expr Expression),
    DeferredList(usize, &'expr Expression),
}

impl<'e> ScopeValue<'e> {
    pub fn value(val: impl Into<Owned>) -> Self {
        Self::Value(ValueRef::Owned(val.into()))
    }
}

impl<'expr, T: Into<ValueRef<'expr>>> From<T> for ScopeValue<'expr> {
    fn from(value: T) -> Self {
        Self::Value(value.into())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Scope<'expr> {
    path: Path<'expr>,
    value: ScopeValue<'expr>,
}

#[derive(Debug, Copy, Clone)]
pub struct Scopes<'frame, 'expr> {
    pub current: Scope<'expr>,
    pub parent: Option<&'frame Scopes<'frame, 'expr>>,
}

impl<'frame, 'expr> Scopes<'frame, 'expr> {
    pub fn get(&self, path: Path<'_>) -> Option<ScopeValue<'expr>> {
        match self.current.path == path {
            true => Some(self.current.value),
            false => self.parent?.get(path),
        }
    }

    #[must_use]
    pub fn scope_value(
        &'frame self,
        path: impl Into<Path<'expr>>,
        value: impl Into<ScopeValue<'expr>>,
    ) -> Self {
        let scope = Scope {
            path: path.into(),
            value: value.into(),
        };

        Self {
            current: scope,
            parent: Some(self),
        }
    }
}

// "get" order is:
// 1. State
// 2. Scope
// 3. Parent
//
// TODO: right now failing to fetch a value with the `Deferred` resolver
// will qualify the value as "deferred", even if the value doesn't exist in the state.
#[derive(Copy, Clone)]
pub struct InnerContext<'frame, 'expr> {
    state: &'frame dyn State,
    scopes: Option<&'frame Scopes<'frame, 'expr>>,
    parent: Option<&'frame InnerContext<'frame, 'expr>>,
}

impl<'frame, 'expr> InnerContext<'frame, 'expr> {
    /// Scope a value and return the scopes.
    /// Once the scopes are updated, create a new context with the new scopes:
    /// ```
    /// # use anathema_values::Context;
    /// # fn run(context: &Context, value: usize, other_value: usize) {
    /// let mut inner = context.inner();
    /// let scopes = inner.scope("key", value);
    /// let scopes = scopes.scope_value("other_key", other_value);
    /// inner.assign(&scopes);
    /// # }
    /// ```
    pub fn scope(
        &self,
        path: impl Into<Path<'expr>>,
        value: impl Into<ScopeValue<'expr>>,
    ) -> Scopes<'frame, 'expr> {
        let scope = Scope {
            path: path.into(),
            value: value.into(),
        };

        Scopes {
            current: scope,
            parent: self.scopes,
        }
    }

    pub fn assign(&mut self, scopes: &'frame Scopes<'frame, 'expr>) {
        self.scopes = Some(scopes);
    }

    fn pop(&self) -> Option<&Self> {
        self.parent
    }
}

impl Debug for InnerContext<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InnterContext")
            .field("state", &"<state>")
            .field("scopes", &self.scopes)
            .field("parent", &self.parent)
            .finish()
    }
}

// Lookup is done elsewhere as the resolver affects the lifetime, therefore there
// is no lookup function on the scope.
//
// For a deferred resolver everything has the same lifetime as the expressions,
// for an immediate resolver the lifetime can only be that of the frame, during the layout step
#[derive(Copy, Clone, Debug)]
pub struct Context<'frame, 'expr> {
    inner: InnerContext<'frame, 'expr>,
}

impl<'frame, 'expr> Context<'frame, 'expr> {
    pub fn root(state: &'frame dyn State) -> Self {
        Self {
            inner: InnerContext {
                state,
                scopes: None,
                parent: None,
            },
        }
    }

    pub fn with_state(&'frame self, state: &'frame dyn State) -> Self {
        let inner = InnerContext {
            state,
            scopes: None,
            parent: Some(&self.inner),
        };

        Self { inner }
    }

    pub fn inner(&'frame self) -> InnerContext<'frame, 'expr> {
        InnerContext {
            state: self.inner.state,
            scopes: self.inner.scopes,
            parent: self.inner.parent,
        }
    }

    pub fn new_context(&'frame self, inner: InnerContext<'frame, 'expr>) -> Self {
        Self { inner }
    }

    // TODO: rename this
    pub fn lookup(&'frame self) -> ContextRef<'frame, 'expr> {
        ContextRef { inner: &self.inner }
    }
}

pub struct ContextRef<'frame, 'expr> {
    inner: &'frame InnerContext<'frame, 'expr>,
}

impl<'frame, 'expr> ContextRef<'frame, 'expr> {
    pub fn pop(&self) -> Option<Self> {
        Some(Self {
            inner: self.inner.pop()?,
        })
    }

    pub fn state(&self, path: Path<'_>, node_id: &NodeId) -> ValueRef<'frame> {
        self.inner.state.state_get(path, node_id)
    }

    pub fn scopes(&self, path: Path<'_>) -> Option<ScopeValue<'expr>> {
        let scopes = self.inner.scopes?;
        let val = scopes.get(path);
        val
    }
}

impl<'frame, 'expr> From<InnerContext<'frame, 'expr>> for Context<'frame, 'expr> {
    fn from(inner: InnerContext<'frame, 'expr>) -> Self {
        Context { inner }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn scope_value() {
        let context = Context::root(&());
        let mut inner = context.inner();
        let scopes = inner.scope("a", 1);
        let scopes = scopes.scope_value("b", 1);
        inner.assign(&scopes);
        let scopes = inner.scope("a", 2);
        inner.assign(&scopes);

        let context: Context<'_, '_> = inner.into();

        let a = context.lookup().scopes("a".into()).unwrap();
        assert_eq!(a, 2.into());

        let b = context.lookup().scopes("b".into()).unwrap();
        assert_eq!(b, 1.into());
    }
}
