use super::Storage;
use crate::Expression;

// TODO: rename to ExpressionId
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
pub struct Values(Storage<Expression>);

impl Values {
    pub(crate) fn empty() -> Self {
        Self(Storage::empty())
    }

    pub(crate) fn push(&mut self, value: Expression) -> ValueId {
        ValueId(self.0.push(value))
    }

    pub(crate) fn get(&self, index: ValueId) -> Option<&Expression> {
        self.0.get(index.0)
    }
}