pub(super) use self::rcslab::RcSlab;

mod rcslab;

// -----------------------------------------------------------------------------
//   - Entry -
// -----------------------------------------------------------------------------
enum Entry<I, T> {
    Vacant(Option<I>),
    Occupied(T),
}

impl<I, T> Entry<I, T> {
    // Insert an Occupied entry in place of a vacant one.
    fn swap(&mut self, value: T) {
        debug_assert!(matches!(self, Entry::Vacant(_)));
        std::mem::swap(self, &mut Entry::Occupied(value));
    }

    // Create a new occupied entry
    fn occupied(value: T) -> Self {
        Self::Occupied(value)
    }

    // Will panic if the entry is vacant.
    // An entry should never be vacant where this call is involved.
    //
    // This means this method should never be used outside of update calls.
    fn as_occupied_mut(&mut self) -> &mut T {
        match self {
            Entry::Occupied(value) => value,
            Entry::Vacant(_) => unreachable!("invalid state"),
        }
    }
}

// -----------------------------------------------------------------------------
//   - Slab -
// -----------------------------------------------------------------------------
pub(in crate::signals::store) struct Slab<I, T> {
    next_id: Option<I>,
    inner: Vec<Entry<I, T>>,
}

impl<I, T> Slab<I, T>
where
    I: Copy,
    I: From<usize>,
    I: Into<usize>,
{
    pub(super) const fn empty() -> Self {
        Self {
            next_id: None,
            inner: vec![],
        }
    }

    // If there is a `self.next_id` then `take` the id (making it None)
    // and replace the vacant entry at the given index.
    //
    // Write the vacant entry's `next_id` into self.next_id, and
    // finally replace the vacant entry with the occupied value
    pub(super) fn push(&mut self, value: T) -> I {
        match self.next_id.take() {
            Some(index) => {
                let entry = &mut self.inner[index.into()];

                let Entry::Vacant(new_next_id) = entry else {
                    unreachable!("you found a bug with Anathema, please file a bug report")
                };

                self.next_id = new_next_id.take();
                entry.swap(value);
                index
            }
            None => {
                self.inner.push(Entry::occupied(value));
                (self.inner.len() - 1).into()
            }
        }
    }

    // Take (removes) a value out of the slab.
    // Will panic if the slot is not occupied
    pub(super) fn take(&mut self, index: I) -> T {
        let mut entry = Entry::Vacant(self.next_id.take());
        self.next_id = Some(index);
        std::mem::swap(&mut self.inner[index.into()], &mut entry);

        match entry {
            Entry::Occupied(val) => val,
            Entry::Vacant(..) => panic!("removal of vacant entry"),
        }
    }

    // Replace an existing value with a new one
    pub(super) fn update(&mut self, index: I, mut new_value: T) -> T {
        let value = self.inner[index.into()].as_occupied_mut();
        std::mem::swap(value, &mut new_value);
        new_value
    }

    pub(super) fn get(&self, index: I) -> &T {
        match &self.inner[index.into()] {
            Entry::Occupied(val) => val,
            _ => unreachable!()
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.inner.iter().filter_map(|e| match e {
            Entry::Occupied(val) => Some(val),
            Entry::Vacant(_) => None
        })
    }
}

impl<I, T> Slab<I, T>
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
                Entry::Vacant(next) => {
                    let _ = write!(&mut s, "{idx}: vacant ");
                    match next {
                        Some(i) => writeln!(&mut s, "next id: {}", (*i).into()),
                        None => writeln!(&mut s, "no next id"),
                    }
                }
                Entry::Occupied(value) => writeln!(&mut s, "{idx}: {value:?}"),
            };
        }

        let _ = writeln!(&mut s, "---- next id ----");

        let _ = match self.next_id {
            Some(i) => writeln!(&mut s, "next id: {}", i.into()),
            None => writeln!(&mut s, "no next id"),
        };

        s
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn push() {
        let mut slab = Slab::<usize, _>::empty();
        let index = slab.push(123);
        let val = slab.take(index);
        assert_eq!(val, 123);
    }

    #[test]
    fn take() {
        let mut slab = Slab::<usize, _>::empty();
        let index_1 = slab.push(1);
        let _ = slab.take(index_1);
        let index_2 = slab.push(2);
        assert_eq!(index_1, index_2)
    }

    #[test]
    fn update() {
        let mut slab = Slab::<usize, _>::empty();
        let index_1 = slab.push("hello world");
        slab.update(index_1, "updated");
        let s = slab.take(index_1);
        assert_eq!(s, "updated");
    }

    
}
