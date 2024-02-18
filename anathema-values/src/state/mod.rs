// -----------------------------------------------------------------------------
//   - Notes about state -
//   State can either belong to a `View` or be passed to a `View`.
//   Any state owned by a view has to be returned by `View::state()` to be
//   accessible inside the templates.
//
//   State owned by the `View` is referred to as Internal State.
//   State passed to the `View` is External State.
// -----------------------------------------------------------------------------
use crate::signals::ValueRef;
use crate::{NodeId, Path};

// mod value;

/// Represents the internal state of a view
/// ```ignore
/// use anathema::values::State;
///
/// #[derive(State)]
/// struct MyState {
///     value: StateValue<String>,
/// }
/// ```
pub trait State {
    #[doc(hidden)]
    fn state_get(&self, path: Path<'_>, node_id: &NodeId) -> Option<ValueRef>;

    #[doc(hidden)]
    fn get_value(&self, _: &NodeId) -> ValueRef
    where
        Self: Sized,
    {
        panic!()
        // ValueRef::Map(self)
    }
}

/// This exists so you can have a view with a default state of a unit
impl State for () {
    fn state_get(&self, _: Path<'_>, _: &NodeId) -> Option<ValueRef> {
        panic!()
        // ValueRef::Empty
    }
}

// TODO: Can we make this `Copy` as well?
//       This depends if `RemoveKey` is required here or not.
//       TB 2023-11-11
//
//       If all keys can be changed to use the constants created
//       during template parsing this could become `Copy`.
//       However then we need a solution for the `get` function on maps
//       as they still take string for lookups (this is used
//       when getting a value from a state inside a view, where
//       the state contains a map)
#[derive(Debug, Clone, PartialEq)]
pub enum Change {
    Update,
    Push,
    InsertIndex(usize),
    // TODO: is this variant needed?
    InsertKey(String),
    RemoveIndex(usize),
    // TODO: is this variant needed?
    RemoveKey(String),
}
