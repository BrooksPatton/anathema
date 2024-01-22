use std::fmt::{self, Display};
use std::ops::Deref;

/// Path lookup
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PathId(pub usize);

impl From<usize> for PathId {
    fn from(index: usize) -> Self {
        Self(index)
    }
}

impl Deref for PathId {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for PathId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<pid({})>", self.0)
    }
}

// -----------------------------------------------------------------------------
//   - Value path -
//   The path to a value in a given context.
//
//   Key     Key    Key
//   parent .child .name
//
//   Key               Index   Key
//   parent_collection .3     .name
// -----------------------------------------------------------------------------
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Path<'e> {
    /// The key is an index to an ident inside `Constants`
    Key(&'e str),
    /// Index in a collection
    Index(usize),
}

impl fmt::Display for Path<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(key) => write!(f, "K({})", key)?,
            Self::Index(index) => write!(f, "I({})", index)?,
        }

        Ok(())
    }
}

impl From<usize> for Path<'_> {
    fn from(index: usize) -> Self {
        Self::Index(index)
    }
}

impl<'e> From<&'e str> for Path<'e> {
    fn from(s: &'e str) -> Self {
        Self::Key(s)
    }
}
