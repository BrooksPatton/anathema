use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use super::{OwnedKey, RcSlab};

// -----------------------------------------------------------------------------
//   - Shared key -
// -----------------------------------------------------------------------------
#[derive(Debug, Copy, Clone)]
pub(crate) struct SharedKey(usize, OwnedKey);

impl From<SharedKey> for usize {
    fn from(key: SharedKey) -> usize {
        key.0
    }
}

impl From<SharedKey> for OwnedKey {
    fn from(key: SharedKey) -> OwnedKey {
        key.1
    }
}

// -----------------------------------------------------------------------------
//   - Shared storage -
// -----------------------------------------------------------------------------
pub(super) struct Shared {
    inner: RefCell<RcSlab<usize, Box<dyn Any>>>,
}

impl Shared {
    pub(super) const fn empty() -> Self {
        Self {
            inner: RefCell::new(RcSlab::empty()),
        }
    }

    // Get a shared value under the assumption that the value exists.
    // This should only be called if the Rc::strong count is greater than one
    pub(super) fn get(&self, key: SharedKey) -> Rc<Option<Box<dyn Any>>> {
        self.inner.borrow_mut().get(key.into())
    }

    pub(super) fn insert(&self, owned_key: OwnedKey, value: Box<dyn Any>) -> SharedKey {
        let key = self.inner.borrow_mut().push(value);
        SharedKey(key, owned_key)
    }

    pub(super) fn try_evict(&self, key: SharedKey) -> Option<Box<dyn Any>> {
        self.inner.borrow_mut().try_remove(key.into())
    }

    pub fn dump_state(&self) -> String {
        self.inner.borrow().dump_state()
    }
}
