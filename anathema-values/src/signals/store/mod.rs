use std::any::Any;
use std::rc::Rc;

use self::slab::{RcSlab, Slab};
use self::owned::Owned;
use self::shared::Shared;

pub(crate) use owned::OwnedKey;
pub(crate) use shared::SharedKey;

mod slab;
mod owned;
mod shared;

thread_local! {
    static OWNED: Owned = const { Owned::empty() };
    static SHARED: Shared = const { Shared::empty() };
}

pub(crate) fn new_value<T: 'static>(value: T) -> OwnedKey {
    OWNED.with(|owned| owned.push(Box::new(value)))
}

pub(crate) fn drop_value(key: OwnedKey) {
    let _ = OWNED.with(|owned| owned.remove(key));
}

pub(crate) fn get_unique(key: OwnedKey) -> Box<dyn Any> {
    OWNED.with(|owned| owned.unique(key))
}

pub(crate) fn make_shared(owned_key: OwnedKey) -> (SharedKey, Rc<Option<Box<dyn Any>>>) {
    OWNED.with(|owned| {
        match owned.get_shared_key(owned_key) {
            Some(key) => {
                (key, lookup_shared(key))
            }
            None => {
                // Transfer value from OWNED to SHARED
                let value = owned.unique(owned_key);
                SHARED.with(|shared| {
                    let key = shared.insert(owned_key, value);
                    owned.set_as_shared(owned_key, key);
                    let ret = (key, lookup_shared(key));
                    ret
                })
            }
        }
    })
}

// If the strong count is > 1 then use `lookup_shard` instead of `get_shared`
pub(crate) fn lookup_shared(key: SharedKey) -> Rc<Option<Box<dyn Any>>> {
    SHARED.with(|shared| shared.get(key))
}

pub(crate) fn return_owned(key: OwnedKey, value: Box<dyn Any>) {
    OWNED.with(|owned| owned.return_unique_borrow(key, value));
}

pub(crate) fn return_shared(key: SharedKey) {
    if let Some(value) = SHARED.with(|shared| shared.try_evict(key)) {
        return_owned(key.into(), value);
    }
}

pub fn dump_state() -> String {
    use std::fmt::Write;
    let mut string = String::new();
    writeln!(&mut string, "\n\n=== SHARED ===\n{}\n", SHARED.with(|s| s.dump_state()));
    writeln!(&mut string, "=== OWNED ===\n{}\n", OWNED.with(|s| s.dump_state()));
    string
}

// -----------------------------------------------------------------------------
//   - Test functions -
// -----------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod testing {
    pub fn owned_len() -> usize{
        super::OWNED.with(|owned| owned.count_occupied())
    }
}
