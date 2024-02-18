use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

use crate::{NodeId, Static, Path, State, DIRTY_NODES};

#[derive(Debug)]
pub struct StateValue<T> {
    pub(crate) inner: T,
    subscribers: RefCell<Vec<NodeId>>,
}

impl<T> StateValue<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            subscribers: RefCell::new(Vec::new()),
        }
    }

    fn notify(&self) {
        for s in self.subscribers.borrow_mut().drain(..) {
            DIRTY_NODES.with(|nodes| nodes.borrow_mut().push((s.clone(), Change::Update)));
        }
    }

    #[doc(hidden)]
    pub fn state_get(&self, _: Path<'_>, _: &NodeId) -> NameThisType {
        ValueRef::Empty
    }

    pub fn subscribe(&self, subscriber: NodeId) {
        self.subscribers.borrow_mut().push(subscriber);
    }
}

impl<T: Default> StateValue<T> {
    pub fn take(&mut self) -> T {
        self.notify();
        std::mem::take(&mut self.inner)
    }
}

impl<T> Default for StateValue<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            inner: T::default(),
            subscribers: Default::default(),
        }
    }
}

impl<T> Clone for StateValue<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        todo!()
    }
}

impl<T> StateValue<T>
where
    for<'b> &'b T: Into<ValueRef<'b>>,
{
    pub fn get_value(&self, node_id: &NodeId) -> ValueRef<'_> {
        self.subscribe(node_id.clone());
        (&self.inner).into()
    }
}

impl<T> Deref for StateValue<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> DerefMut for StateValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.notify();
        &mut self.inner
    }
}

impl<T> From<T> for StateValue<T> {
    fn from(val: T) -> StateValue<T> {
        StateValue::new(val)
    }
}

impl<'a> From<&'a StateValue<String>> for ValueRef<'a> {
    fn from(value: &'a StateValue<String>) -> Self {
        ValueRef::Str(value.inner.as_str())
    }
}

impl<'a, T> From<&'a StateValue<T>> for ValueRef<'a>
where
    Static: From<&'a T>,
{
    fn from(value: &'a StateValue<T>) -> Self {
        ValueRef::Owned((&value.inner).into())
    }
}

impl<T: State> State for StateValue<T> {
    fn state_get(&self, key: Path<'_>, node_id: &NodeId) -> ValueRef<'_> {
        self.inner.state_get(key, node_id)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::drain_dirty_nodes;

    #[test]
    fn notify_subscriber() {
        let id: NodeId = 123.into();
        let mut value = StateValue::new("hello world".to_string());
        value.subscribe(id.clone());
        value.push_str(", updated");

        assert_eq!((id, Change::Update), drain_dirty_nodes()[0]);
    }
}
