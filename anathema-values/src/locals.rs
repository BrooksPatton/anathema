use std::rc::Rc;

use crate::{Expression, Map, NodeId, Path, Deferred};

#[derive(Debug, Default)]
pub struct Locals {
    map: Map<Expression>,
}

impl Locals {
    pub fn get(&self, path: Path<'_>, node_id: &NodeId) -> Option<&Expression> {
        match path {
            Path::Key(key) => self.map.get(key),
            Path::Index(_) => None,
        }
    }

    pub fn declare(&mut self, binding: Rc<str>, rhs: Expression) {
        self.map.insert(binding.to_string(), rhs);
    }
}
