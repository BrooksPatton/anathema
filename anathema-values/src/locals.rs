use crate::{Expression, Map, NodeId, Path};

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

    pub fn assign(&mut self, lhs: &Expression, rhs: &Expression) {
    }
}
