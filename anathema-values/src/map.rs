use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

use crate::hashmap::HashMap;
use crate::state::State;
use crate::{Change, Collection, NodeId, Path, StateValue, ValueRef, DIRTY_NODES};

#[derive(Debug)]
pub struct Map<T> {
    inner: HashMap<String, StateValue<T>>,
    subscribers: RefCell<Vec<NodeId>>,
}

impl<T> Map<T> {
    pub fn empty() -> Self {
        Self::new::<String>(HashMap::new())
    }

    pub fn new<K: Into<String>>(inner: impl IntoIterator<Item = (K, T)>) -> Self {
        let inner = inner
            .into_iter()
            .map(|(k, v)| (k.into(), StateValue::new(v)));
        Self {
            inner: HashMap::from_iter(inner),
            subscribers: RefCell::new(vec![]),
        }
    }

    pub fn subscribe(&self, node_id: NodeId) {
        self.subscribers.borrow_mut().push(node_id);
    }

    pub fn remove(&mut self, key: String) -> Option<StateValue<T>> {
        let ret = self.inner.remove(&key);
        for s in self.subscribers.borrow_mut().drain(..) {
            DIRTY_NODES.with(|nodes| {
                nodes
                    .borrow_mut()
                    .push((s.clone(), Change::RemoveKey(key.clone())))
            });
        }
        ret
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.inner.insert(key.clone(), StateValue::new(value));
        for s in self.subscribers.borrow_mut().drain(..) {
            DIRTY_NODES.with(|nodes| {
                nodes
                    .borrow_mut()
                    .push((s.clone(), Change::InsertKey(key.clone())))
            });
        }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.inner.get(key).map(|v| &v.inner)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut T> {
        self.inner.get_mut(key).map(|v| v.deref_mut())
    }
}

impl<T> Default for Map<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Map<T>
where
    for<'a> &'a T: Into<ValueRef<'a>>,
{
    pub fn get_value(&self, node_id: &NodeId) -> ValueRef<'_> {
        self.subscribe(node_id.clone());
        ValueRef::Map(self)
    }
}

impl<T> Collection for Map<T>
where
    for<'a> &'a T: Into<ValueRef<'a>>,
{
    fn len(&self) -> usize {
        self.inner.len()
    }

    fn subscribe(&self, _node_id: NodeId) {
        todo!()
    }
}

impl<T> State for Map<T>
where
    for<'a> &'a T: Into<ValueRef<'a>>,
{
    fn state_get(&self, path: Path<'_>, node_id: &NodeId) -> ValueRef<'_> {
        match path {
            Path::Key(key) => {
                let Some(value) = self.inner.get(key) else {
                    return ValueRef::Empty;
                };
                value.subscribe(node_id.clone());
                value.deref().into()
            }
            Path::Index(_) => ValueRef::Empty,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::testing::TestState;
    use crate::Static;

    #[test]
    fn access_map() {
        let state = TestState::new();
        let node_id = 0.into();

        let ValueRef::Map(map) = state.state_get("generic_map".into(), &node_id) else {
            panic!()
        };
        let ValueRef::Map(map) = state.state_get("inner".into(), &node_id) else {
            panic!()
        };
        let ValueRef::Str(name) = map.state_get("name".into(), &node_id) else {
            panic!()
        };
        assert_eq!(name, &*state.inner.name);
    }
}
