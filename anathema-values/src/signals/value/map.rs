use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

use super::ValueRef;
use crate::signals::Value;
use crate::{NodeId, Path, State};

#[derive(Debug)]
pub struct Map<T> {
    inner: HashMap<String, Value<T>>,
}

impl<T: 'static> Map<T> {
    pub fn new<K: Into<String>>(inner: impl IntoIterator<Item = (K, T)>) -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&Value<T>>
    where
        String: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.inner.get(k)
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.inner.insert(key, Value::new(value));
    }

    pub fn remove(&mut self, key: &str) -> Option<Value<T>> {
        self.inner.remove(key)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<T> State for Map<T> {
    fn state_get(&self, path: Path<'_>, node_id: &NodeId) -> Option<ValueRef> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::testing::owned_len;
}
