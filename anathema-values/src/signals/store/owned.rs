use std::any::Any;
use std::cell::RefCell;

use super::shared::SharedKey;
use super::Slab;

// -----------------------------------------------------------------------------
//   - Key -
// -----------------------------------------------------------------------------
#[derive(Debug, Copy, Clone)]
pub(crate) struct OwnedKey(usize);

impl From<usize> for OwnedKey {
    fn from(i: usize) -> Self {
        Self(i)
    }
}

impl Into<usize> for OwnedKey {
    fn into(self) -> usize {
        self.0
    }
}

// -----------------------------------------------------------------------------
//   - Storage entity -
// -----------------------------------------------------------------------------
#[derive(Debug)]
enum OwnedEntry {
    Occupied(Box<dyn Any>),
    Unique,
    Shared(SharedKey),
}

impl OwnedEntry {
    fn is_occupied(&self) -> bool {
        match self {
            Self::Occupied(_) => true,
            _ => false,
        }
    }
}

// -----------------------------------------------------------------------------
//   - Storage -
// -----------------------------------------------------------------------------
pub(super) struct Owned {
    inner: RefCell<Slab<OwnedKey, OwnedEntry>>,
}

impl Owned {
    pub(super) const fn empty() -> Self {
        Self {
            inner: RefCell::new(Slab::empty()),
        }
    }

    // This assumes the value already exists.
    pub(super) fn get_shared_key(&self, key: OwnedKey) -> Option<SharedKey> {
        match self.inner.borrow().get(key) {
            OwnedEntry::Shared(key) => Some(*key),
            _ => None,
        }
    }

    pub(super) fn push(&self, value: Box<dyn Any>) -> OwnedKey {
        self.inner.borrow_mut().push(OwnedEntry::Occupied(value))
    }

    pub(super) fn set_as_shared(&self, owned_key: OwnedKey, shared_key: SharedKey) {
        match self
            .inner
            .borrow_mut()
            .update(owned_key, OwnedEntry::Shared(shared_key))
        {
            OwnedEntry::Unique => {}
            _ => panic!("invalid state"),
        }
    }

    pub(super) fn unique(&self, key: OwnedKey) -> Box<dyn Any> {
        let mut inner = self.inner.borrow_mut();
        let output = inner.update(key, OwnedEntry::Unique);
        match output {
            OwnedEntry::Occupied(value) => value,
            entry => panic!("invalid state, {entry:?}"),
        }
    }

    // Remove the value from the storage
    pub(super) fn remove(&self, key: OwnedKey) -> Box<dyn Any> {
        match self.inner.borrow_mut().take(key) {
            OwnedEntry::Occupied(value) => value,
            _ => panic!("invalid state"),
        }
    }

    // Return a value to the storage.
    // The value can be either a
    // * Unique borrow
    // * The end of a shared borrow
    pub(super) fn return_unique_borrow(&self, key: OwnedKey, value: Box<dyn Any>) {
        let val = self
            .inner
            .borrow_mut()
            .update(key, OwnedEntry::Occupied(value));
        match val {
            OwnedEntry::Unique => (),
            OwnedEntry::Shared(_) => (),
            _ => panic!("invalid state"),
        }
    }

    pub(super) fn count_occupied(&self) -> usize {
        self.inner.borrow().iter().filter(|e| e.is_occupied()).count()
    }

    pub fn dump_state(&self) -> String {
        self.inner.borrow().dump_state()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn push() {
        let mut owned = Owned::empty();
        let key = owned.push(Box::new(123u32));
        let unique = owned.unique(key);
        let value: u32 = *Box::<dyn Any>::downcast(unique).unwrap();
        assert_eq!(value, 123);
    }

    #[test]
    #[should_panic(expected = "invalid state")]
    fn unique_borrow() {
        let mut owned = Owned::empty();
        let key = owned.push(Box::new(123u32));
        let _ = owned.unique(key);
        let _ = owned.unique(key);
    }

    #[test]
    fn return_unique_borrow() {
        let mut owned = Owned::empty();
        let key = owned.push(Box::new(123u32));
        let value = owned.unique(key);
        owned.return_unique_borrow(key, value);
        let value = owned.unique(key);
    }

    #[test]
    #[should_panic(expected = "invalid state")]
    fn remove() {
        let mut owned = Owned::empty();
        let key = owned.push(Box::new(123u32));
        let _ = owned.remove(key);
        let value = owned.unique(key);
    }
}
