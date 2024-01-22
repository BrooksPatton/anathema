use super::Storage;
use crate::ValueExpr;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ValueId(usize);

impl From<usize> for ValueId {
    fn from(n: usize) -> Self {
        Self(n)
    }
}

impl From<ValueId> for usize {
    fn from(value: ValueId) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub struct Values(Storage<ValueExpr>);

impl Values {
    pub(crate) fn empty() -> Self {
        Self(Storage::empty())
    }

    pub(crate) fn push(&mut self, value: ValueExpr) -> ValueId {
        ValueId(self.0.push(value))
    }

    pub(crate) fn get(&self, index: ValueId) -> Option<&ValueExpr> {
        self.0.get(index.0)
    }
}
