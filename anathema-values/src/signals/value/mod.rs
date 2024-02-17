use std::any::Any;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub use list::List;

use crate::signals::store::{
    drop_value, get_unique, make_shared, new_value, return_owned, return_shared, OwnedKey,
    SharedKey,
};
use crate::{NodeId, DIRTY_NODES, Change};

mod list;
mod map;

// -----------------------------------------------------------------------------
//   - Value -
// -----------------------------------------------------------------------------
#[derive(Debug)]
pub struct Value<T> {
    key: OwnedKey,
    subscribers: Vec<NodeId>,
    _p: PhantomData<T>,
}

impl<T: 'static> From<T> for Value<T> {
    fn from(value: T) -> Self {
        Value::new(value)
    }
}

impl<T: 'static> Value<T> {
    pub fn new(value: T) -> Self {
        let key = new_value(value);

        Self {
            key,
            subscribers: vec![],
            _p: PhantomData,
        }
    }

    pub fn to_mut(&mut self) -> Unique<'_, T> {
        let value = get_unique(self.key);
        Unique {
            value: Some(value),
            key: self.key,
            subscribers: &mut self.subscribers,
            _p: PhantomData,
        }
    }

    pub fn to_ref(&self) -> Shared<'_, T> {
        let (key, value) = make_shared(self.key);
        Shared {
            value: Some(value),
            key,
            _p: PhantomData,
        }
    }

    pub fn value_ref(&mut self, node_id: NodeId) -> ValueRef {
        self.subscribers.push(node_id);

        ValueRef { key: self.key }
    }
}

impl<T> Drop for Value<T> {
    fn drop(&mut self) {
        drop_value(self.key);
    }
}

// -----------------------------------------------------------------------------
//   - Unique -
// -----------------------------------------------------------------------------
pub struct Unique<'a, T: 'static> {
    value: Option<Box<dyn Any>>,
    key: OwnedKey,
    subscribers: &'a mut Vec<NodeId>,
    _p: PhantomData<&'a mut T>,
}

impl<'a, T> Unique<'a, T> {
    fn notify(&mut self) {
        for s in self.subscribers.drain(..) {
            DIRTY_NODES.with(|nodes| nodes.borrow_mut().push((s.clone(), Change::Update)));
        }
    }
}

impl<'a, T> Deref for Unique<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
            .as_ref()
            .expect("value is only ever set to None on drop")
            .downcast_ref()
            .expect("the type should never change")
    }
}

impl<'a, T> DerefMut for Unique<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.notify();

        self.value
            .as_mut()
            .expect("value is only ever set to None on drop")
            .downcast_mut()
            .expect("the type should never change")
    }
}

impl<'a, T: 'static> Drop for Unique<'a, T> {
    fn drop(&mut self) {
        // TODO this can be an unwrap_unchecked because the `value` is always Some(_) in `Unique`
        //      except here where it's dropped
        let value = self.value.take().unwrap();

        // this is the only place where self.value = None
        return_owned(self.key, value);
    }
}

// -----------------------------------------------------------------------------
//   - Shared -
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct Shared<'a, T: 'static> {
    value: Option<Rc<Option<Box<dyn Any>>>>,
    key: SharedKey,
    _p: PhantomData<&'a T>,
}

impl<'a, T> Deref for Shared<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
            .as_ref()
            .unwrap()
            .as_ref()
            .as_ref()
            .and_then(|b| b.downcast_ref())
            .unwrap()
    }
}

impl<T: Debug> Debug for Shared<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl<T> Drop for Shared<'_, T> {
    fn drop(&mut self) {
        drop(self.value.take());
        return_shared(self.key);
    }
}

// -----------------------------------------------------------------------------
//   - Value ref -
// -----------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ValueRef {
    key: OwnedKey,
}

impl ValueRef {
    pub fn val<T: 'static>(&self) -> Shared<'_, T> {
        let (key, value) = make_shared(self.key);
        Shared {
            value: Some(value),
            key,
            _p: PhantomData,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn new_value() {
        let mut value = Value::new("hello world");
        let unique = value.to_mut();
        assert_eq!("hello world", *unique);
    }

    #[test]
    fn mutable_access() {
        let mut value = Value::new(String::new());
        {
            let mut unique = value.to_mut();
            unique.push_str("updated");
        }

        let mut unique = value.to_mut();
        assert_eq!("updated", *unique);
    }

    #[test]
    fn shared_access() {
        let expected = "hello world";
        let value = Value::new(expected);
        let s1 = value.to_ref();
        let s2 = value.to_ref();

        assert_eq!(*s1, expected);
        assert_eq!(*s2, expected);
    }

    #[test]
    #[should_panic(expected = "invalid state")]
    fn mutable_shared_panic() {
        let mut value = Value::new(String::new());
        let s1 = value.value_ref();
        let m1 = value.to_mut();
        let r1 = s1.val();
    }
}
