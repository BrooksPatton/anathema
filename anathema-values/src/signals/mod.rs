mod store;
mod value;

#[cfg(test)]
pub(crate) use store::testing;

pub use value::{Value, ValueRef, List};
