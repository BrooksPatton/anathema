use std::collections::VecDeque;
use std::ops::{Index, IndexMut};

use crate::{NodeId, Path, State, ValueRef};
use crate::signals::Value;

pub struct List<T> {
    inner: VecDeque<Value<T>>,
}

impl<T: 'static> List<T> {
    pub fn new<U: Into<T>>(values: impl IntoIterator<Item = U>) -> Self {
        let inner = values.into_iter().map(|v| Value::new(v.into())).collect();
        Self { inner }
    }

    pub fn push_back(&mut self, value: T) {
        self.inner.push_back(Value::new(value));
    }

    pub fn push_front(&mut self, value: T) {
        self.inner.push_front(Value::new(value));
    }

    pub fn remove(&mut self, index: usize) -> Option<Value<T>> {
        self.inner.remove(index)
    }

    pub fn insert(&mut self, index: usize, value: T) {
        self.inner.insert(index, Value::new(value));
    }

    pub fn swap(&mut self, from: usize, to: usize) {
        self.inner.swap(from, to);
    }

    pub fn clear(&mut self) {
        self.inner.clear();
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<T> Index<usize> for List<T> {
    type Output = Value<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl<T> IndexMut<usize> for List<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.inner[index]
    }
}

impl<T> State for List<T> {
    fn state_get(&self, path: Path<'_>, node_id: &NodeId) -> ValueRef<'_> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::testing::owned_len;

    #[test]
    fn insert() {
        let list = List::<u32>::new(0..10u32);
        assert_eq!(list.len(), owned_len());
        assert_eq!(owned_len(), 10);
    }

    #[test]
    fn clear() {
        let mut list = List::<u32>::new(0..10u32);
        list.clear();
        assert_eq!(list.len(), owned_len());
        assert_eq!(owned_len(), 0);
        // panic!("{}", crate::dump_state());
    }

    #[test]
    fn swapsies() {
        let mut list = List::<u32>::new(0..10u32);
        let first = &list[0];
        let last = &list[9];
        assert_eq!(0, *first.to_ref());
        assert_eq!(9, *last.to_ref());

        list.swap(0, 9);

        let first = &list[0];
        let last = &list[9];
        assert_eq!(9, *first.to_ref());
        assert_eq!(0, *last.to_ref());
    }
}
