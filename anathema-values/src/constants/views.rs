use std::fmt::Display;

use super::Storage;

// TODO: maybe not make this public?
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ViewId(pub usize);

impl ViewId {
    pub fn root() -> Self {
        Self(usize::MAX)
    }
}

impl From<usize> for ViewId {
    fn from(n: usize) -> Self {
        Self(n)
    }
}

impl Display for ViewId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<vid {}>", self.0)
    }
}

#[derive(Debug)]
pub struct ViewIds {
    storage: Storage<String>,
    root: ViewId,
}

impl ViewIds {
    pub fn new() -> Self {
        let storage = Storage::empty();
        let root = ViewId::root();
        Self { storage, root }
    }

    pub fn push(&mut self, string: String) -> ViewId {
        ViewId(self.storage.push(string))
    }

    pub fn root_id(&self) -> ViewId {
        self.root
    }
}
