use std::mem::swap;
use std::rc::Rc;

// -----------------------------------------------------------------------------
//   - Entry -
//   Both vacant and occupied use the same Rc to prevent additional allocations
// -----------------------------------------------------------------------------
enum Entry<I, T> {
    Pending,
    // The `Rc<Option<T>>` should always be `None`
    Vacant(Option<I>, Rc<Option<T>>),
    // The `Rc<Option<T>>` should always be `Some(T)`
    Occupied(Rc<Option<T>>),
}

impl<I, T> Entry<I, T> {
    // Insert an Occupied entry in place of a vacant one.
    fn swap(&mut self, inner_value: T) {
        debug_assert!(matches!(self, Entry::Vacant(..)));

        let mut entry = Entry::Pending;
        swap(&mut entry, self);

        match entry {
            Entry::Vacant(_, mut storage_cell) => {
                Rc::get_mut(&mut storage_cell)
                    .expect("Rc strong count is always one here")
                    .replace(inner_value);

                swap(
                    self,
                    &mut Entry::Occupied(storage_cell),
                );
            }
            _ => unreachable!(),
        }
    }

    // Try to make the entry vacant and return the value
    fn try_evict(&mut self, next_id: &mut Option<I>) -> Option<T> {
        // If the strong count is anything but 1, then return None
        if let Entry::Occupied(value) = self {
            if Rc::strong_count(value) != 1 {
                return None;
            }
        }

        let mut value = Entry::Pending;
        swap(&mut value, self);

        match value {
            Entry::Occupied(mut store) => {
                let value = Rc::get_mut(&mut store)
                    .expect("strong count is always one")
                    .take()
                    .expect("occupied variant never contains a None");
                swap(self, &mut Entry::Vacant(next_id.take(), store));
                Some(value)
            }
            _ => unreachable!(),
        }
    }

    // Create a new occupied entry
    fn allocate_occupied(value: T) -> Self {
        Self::Occupied(Rc::new(Some(value)))
    }
}

// -----------------------------------------------------------------------------
//   - Rc backed slab -
// -----------------------------------------------------------------------------
pub(in crate::signals::store) struct RcSlab<I, T> {
    next_id: Option<I>,
    inner: Vec<Entry<I, T>>,
}

impl<I, T> RcSlab<I, T>
where
    I: Copy,
    I: From<usize>,
    I: Into<usize>,
    T: std::fmt::Debug
{
    pub(in crate::signals::store) const fn empty() -> Self {
        Self {
            next_id: None,
            inner: vec![],
        }
    }

    // This will clone the underlying Rc and incremenent the count.
    // Unlike the `Slab` which can only have values inserted and removed
    // the `RcSlab` needs the values to be manually removed with `try_remove`.
    pub(in crate::signals::store) fn get(&mut self, index: I) -> Rc<Option<T>> {
        match &self.inner[index.into()] {
            Entry::Occupied(value) => value.clone(),
            _ => unreachable!()
        }
    }

    // If there is a `self.next_id` then `take` the id (making it None)
    // and replace the vacant entry at the given index.
    //
    // Write the vacant entry's `next_id` into self.next_id, and
    // finally replace the vacant entry with the occupied value
    pub(in crate::signals::store) fn push(&mut self, value: T) -> I {
        match self.next_id.take() {
            Some(index) => {
                let entry = &mut self.inner[index.into()];

                let Entry::Vacant(new_next_id, _) = entry else {
                    unreachable!("you found a bug with Anathema, please file a bug report")
                };

                self.next_id = new_next_id.take();
                entry.swap(value);
                index
            }
            None => {
                self.inner.push(Entry::allocate_occupied(value));
                (self.inner.len() - 1).into()
            }
        }
    }

    // Take a value out of the slab.
    // Will panic if the slot is not occupied
    pub(in crate::signals::store) fn try_remove(&mut self, index: I) -> Option<T> {
        let value = self.inner[index.into()].try_evict(&mut self.next_id);
        if value.is_some() {
            self.next_id = Some(index);
        }
        value
    }
}

impl<I, T> RcSlab<I, T>
where
    I: Copy,
    I: From<usize>,
    I: Into<usize>,
    T: std::fmt::Debug
{

    pub fn dump_state(&self) -> String {
        use std::fmt::Write;

        let mut s = String::new();

        for (idx, value) in self.inner.iter().enumerate() {
            let _ = match value {
                Entry::Pending => writeln!(&mut s, "{idx}: pending"),
                Entry::Vacant(next, value) => {
                    let count = Rc::strong_count(&value);
                    let _ = write!(&mut s, "{idx}: value: {value:?} | count: {count} | ");
                    match next {
                        Some(i) => writeln!(&mut s, "next id: {}", (*i).into()),
                        None => writeln!(&mut s, "no next id"),
                    }
                }
                Entry::Occupied(value) => writeln!(&mut s, "{idx}: {value:?} | count: {}", Rc::strong_count(value)),
            };
        }

        let _ = writeln!(&mut s, "---- next id ----");

        let _ = match self.next_id {
            Some(i) => {
                writeln!(&mut s, "next id: {}", i.into())
            },
            None => writeln!(&mut s, "no next id"),
        };

        s
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Lol<T> {
        inner: Option<Rc<Option<T>>>,
    }

    impl<T> Drop for Lol<T> {
        fn drop(&mut self) {
            let _ = self.inner.take().unwrap();
        }
    }

    #[test]
    fn try_remove_value() {
        let mut slab = RcSlab::<usize, _>::empty();
        let index = slab.push("I has a value");

        {
            let value_1 = Lol {
                inner: Some(slab.get(index)),
            };
            assert!(slab.try_remove(index).is_none());
        }

        assert!(slab.try_remove(index).is_some());
    }

    #[test]
    fn ensure_rc_resuse() {
        let mut slab = RcSlab::<usize, _>::empty();
        let index = slab.push(123);
        assert!(slab.try_remove(index).is_some());

        let ptr_a = {
            let Entry::Vacant(_, rc) = &slab.inner[0] else {
                panic!()
            };
            Rc::as_ptr(&rc)
        };

        let index = slab.push(456);
        let Entry::Occupied(value) = &slab.inner[0] else {
            panic!()
        };
        let ptr_b = Rc::as_ptr(&value);

        assert_eq!(ptr_a, ptr_b);
    }

    #[test]
    fn push_multi() {
        let mut slab = RcSlab::<usize, usize>::empty();
        let idx1 = slab.push(1);
        let idx2 = slab.push(2);
        let idx3 = slab.push(3);

        assert_eq!(0, idx1);
        assert_eq!(1, idx2);
        assert_eq!(2, idx3);
    }

    #[test]
    fn clones() {
        let mut slab = RcSlab::<usize, usize>::empty();
        // Count = 1
        let idx1 = slab.push(1);

        {
            // Count = 3
            let val1 = slab.get(idx1);
            let val2 = slab.get(idx1);
            let Entry::Occupied(val) = &slab.inner[0] else { panic!() };
            assert_eq!(Rc::strong_count(val), 3);
        }

        let Entry::Occupied(val) = &slab.inner[0] else { panic!() };
        assert_eq!(Rc::strong_count(val), 1);
    }
}
