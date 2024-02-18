use std::fmt::Debug;

use crate::expressions::NameThisType;
use crate::hashmap::HashMap;
use crate::state::State;
use crate::{Expression, Map, NodeId, Path, Static, ValueRef};

// "get" order is:
// 1. State
// 2. Scope
// 3. Parent
//
// TODO: right now failing to fetch a value with the `Deferred` resolver
// will qualify the value as "deferred", even if the value doesn't exist in the state.
#[derive(Copy, Clone)]
pub struct InnerContext<'frame> {
    state: &'frame dyn State,
    parent: Option<&'frame InnerContext<'frame>>,
}

impl Debug for InnerContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InnterContext")
            .field("state", &"<state>")
            .field("parent", &self.parent)
            .finish()
    }
}

// Lookup is done elsewhere as the resolver affects the lifetime, therefore there
// is no lookup function on the scope.
//
// For a deferred resolver everything has the same lifetime as the expressions,
// for an immediate resolver the lifetime can only be that of the frame, during the layout step
#[derive(Debug)]
pub struct Context<'frame> {
    inner: InnerContext<'frame>,
}

impl<'frame> Context<'frame> {
    pub fn root(state: &'frame dyn State) -> Self {
        Self {
            inner: InnerContext {
                state,
                parent: None,
            },
        }
    }

    // TODO: remove this
    pub fn from_state(&'frame self, state: &'frame dyn State) -> Self {
        let inner = InnerContext {
            state,
            parent: Some(&self.inner),
        };

        Self { inner }
    }

    // TODO: remove this
    pub fn inner(&'frame self) -> InnerContext<'frame> {
        InnerContext {
            state: self.inner.state,
            parent: self.inner.parent,
        }
    }

    // TODO: rename this
    pub fn lookup(&'frame self) -> ContextRef<'frame> {
        ContextRef { inner: &self.inner }
    }
}

pub struct ContextRef<'frame> {
    inner: &'frame InnerContext<'frame>,
}

impl<'frame> ContextRef<'frame> {
    pub fn state(&self, path: Path<'_>, node_id: &NodeId) -> Option<ValueRef> {
        self.inner.state.state_get(path, node_id)
    }

    // pub fn scopes(&self, path: Path<'_>) -> Option<ScopeValue<'expr>> {
    //     let scopes = self.inner.scopes?;
    //     let val = scopes.get(path);
    //     val
    // }
}

impl<'frame> From<InnerContext<'frame>> for Context<'frame> {
    fn from(inner: InnerContext<'frame>) -> Self {
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

        let context: Context<'_> = inner.into();

        let a = context.lookup().scopes("a".into()).unwrap();
        assert_eq!(a, 2.into());

        let b = context.lookup().scopes("b".into()).unwrap();
        assert_eq!(b, 1.into());
    }
}
