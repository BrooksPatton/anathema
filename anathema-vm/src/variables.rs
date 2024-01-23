use std::rc::Rc;

use anathema_values::hashmap::HashMap;
use anathema_values::{Owned, Slab, StringId, Expression, ValueId};

use crate::error::{Error, Result};

const INDENT: usize = 4;

/// The scope id acts as a path made up of indices
/// into the scope tree.
/// E.g `[0, 1, 0]` would point to `root.children[0].children[1].children[0]`.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ScopeId(Rc<[u16]>);

impl ScopeId {
    // Create the next child id.
    fn next(&self, index: u16) -> Self {
        let mut scope_id = Vec::with_capacity(self.0.len() + 1);
        scope_id.extend_from_slice(&self.0);
        scope_id.push(index);
        Self(scope_id.into())
    }

    // Get the parent id as a slice.
    fn parent(&self) -> &[u16] {
        // Can't get the parent of the root
        debug_assert!(self.0.len() > 1);

        let to = self.0.len() - 1;
        &self.0[..to]
    }

    // Check if either `id` or `self` is a sub path of the other.
    // If it is, return the length of the shortest of the two.
    fn sub_path_len(&self, id: impl AsRef<[u16]>) -> Option<usize> {
        let id = id.as_ref();
        let len = id.len().min(self.0.len());
        let lhs = &self.0[..len];
        let rhs = &id[..len];
        (lhs == rhs).then_some(len)
    }

    fn as_slice(&self) -> &[u16] {
        &self.0
    }

    // Does other contain self
    fn contains(&self, other: impl AsRef<[u16]>) -> Option<&ScopeId> {
        let other = other.as_ref();
        let len = self.0.len();

        match other.len() >= len {
            true => (&*self.0 == &other[..len]).then_some(self),
            false => None,
        }
    }
}

impl AsRef<[u16]> for ScopeId {
    fn as_ref(&self) -> &[u16] {
        &self.0
    }
}

impl From<&[u16]> for ScopeId {
    fn from(value: &[u16]) -> Self {
        Self(value.into())
    }
}

impl<const N: usize> From<[u16; N]> for ScopeId {
    fn from(value: [u16; N]) -> Self {
        Self(value.into())
    }
}

#[derive(Debug)]
struct RootScope(Scope);

impl RootScope {
    pub fn new() -> Self {
        Self(Scope::new(ScopeId(vec![0].into())))
    }

    pub fn id(&self) -> &ScopeId {
        &self.0.id
    }

    fn create_child(&mut self) -> ScopeId {
        self.0.create_child()
    }

    fn insert(&mut self, ident: StringId, value: ValueId) {
        self.0.insert(ident, value)
    }

    fn insert_at(&mut self, ident: StringId, value: ValueId, id: impl AsRef<[u16]>) {
        let scope = self.get_scope_mut(id);
        scope.insert(ident, value)
    }

    fn get_scope_mut(&mut self, id: impl AsRef<[u16]>) -> &mut Scope {
        let mut scope = &mut self.0;
        let mut id = &id.as_ref()[1..];

        while !id.is_empty() {
            scope = &mut scope.children[id[0] as usize];
            id = &id[1..];
        }

        scope
    }

    // Get the value id "closest" to the given scope id.
    //
    // e.g
    // ident0 @ scope [0]
    // ident1 @ scope [0, 0]
    // ident2 @ scope [0, 1]
    // ident3 @ scope [0, 1, 1]
    //
    // given an id of [0, 1, 1, 2, 3] would find `ident3` as the closest.
    //
    // If there is no value with the given ident within reach
    // then return `None`.
    fn get_value_id(&self, id: impl AsRef<[u16]>, ident: StringId) -> Option<ValueId> {
        let mut scope = &self.0;
        let mut id = &id.as_ref()[1..];
        let mut value = self
            .0
            .values
            .get(&ident)
            .and_then(|values| values.last())
            .copied();

        while !id.is_empty() {
            scope = &scope.children[id[0] as usize];
            id = &id[1..];

            if let val @ Some(_) = scope
                .values
                .get(&ident)
                .and_then(|values| values.last())
                .copied()
            {
                value = val;
            }
        }

        value
    }
}

/// A scope stores versioned values
#[derive(Debug)]
pub struct Scope {
    values: HashMap<StringId, Vec<ValueId>>,
    id: ScopeId,
    children: Vec<Scope>,
}

impl Scope {
    fn new(id: ScopeId) -> Self {
        Self {
            id,
            values: Default::default(),
            children: vec![],
        }
    }

    fn print(&self, level: usize) {
        let indent = " ".repeat(level * INDENT);
        eprintln!("{indent}{:?}", self.id);

        for child in &self.children {
            child.print(level + 1);
        }
    }

    // Create the next child scope id.
    // ```
    // let mut current = ScopeId::from([0]);
    // let next = current.next_scope(); // scope 0,0
    // let next = current.next_scope(); // scope 0,1
    // ```
    fn create_child(&mut self) -> ScopeId {
        let index = self.children.len();
        let id = self.id.next(index as u16);
        self.children.push(Scope::new(id.clone()));
        id
    }

    // Every call to `insert` will shadow the previous value, not replace it.
    fn insert(&mut self, ident: StringId, value: ValueId) {
        let entry = self.values.entry(ident).or_default();
        entry.push(value);
    }
}

#[derive(Debug)]
struct Declarations(HashMap<StringId, Vec<(ScopeId, ValueId)>>);

impl Declarations {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, ident: StringId, id: impl Into<ScopeId>, value_id: impl Into<ValueId>) {
        let value_id = value_id.into();
        let ids = self.0.entry(ident).or_default();
        ids.push((id.into(), value_id));
    }

    // Get the scope id that is closest to the argument
    fn get(&self, ident: StringId, id: impl AsRef<[u16]>) -> Option<(&ScopeId, ValueId)> {
        self.0
            .get(&ident)
            .unwrap()
            .iter()
            .rev()
            .filter_map(|(scope, value)| scope.contains(&id).map(|s| (s, *value)))
            .next()
    }

    #[cfg(test)]
    fn get_ref(&self, ident: StringId, id: impl AsRef<[u16]>) -> &[u16] {
        self.get(ident, id).unwrap().0.as_ref()
    }
}

/// Variable access, declaration and assignment
/// during the compilation step.
#[derive(Debug)]
pub struct Variables {
    root: RootScope,
    current: ScopeId,
    store: Slab<Expression>,
    declarations: Declarations,
}

impl Variables {
    pub fn new() -> Self {
        let root = RootScope::new();
        Self {
            current: root.0.id.clone(),
            root,
            store: Slab::empty(),
            declarations: Declarations::new(),
        }
    }

    fn declare_at(&mut self, ident: StringId, value_id: ValueId, id: ScopeId) -> ValueId {
        let scope = self.root.get_scope_mut(id);
        scope.insert(ident, value_id);
        self.declarations.add(ident, scope.id.clone(), value_id);
        value_id
    }

    pub fn declare(&mut self, ident: StringId, value: Expression) -> ValueId {
        let value_id = self.store.push(value).into();
        let scope_id = self.current.clone();
        self.declare_at(ident, value_id, scope_id)
    }

    // TODO: check if the value is declared and within reach,
    // else return an error
    pub fn assign(&mut self, ident: StringId, value: Expression) -> Result<ValueId> {
        let (decl_scope_id, decl_value_id) = self.declarations.get(ident, &self.current).unwrap();

        let value_id = self.store.push(value).into();
        let scope = self.root.get_scope_mut(&self.current.0);
        scope.insert(ident, value_id);

        if decl_scope_id < &self.current {
            // insert dyn value with the new value id and the original value id
            let dyn_value = panic!(); //Value2::Dyn(decl_value_id, value_id);
            let dyn_value_id = self.store.push(dyn_value).into();
            self.declare_at(ident, dyn_value_id, decl_scope_id.clone());
        }

        Ok(value_id)
    }

    /// Fetch a value starting from the current path.
    pub fn fetch(&self, ident: StringId) -> Option<Expression> {
        self.root
            .get_value_id(&self.current, ident)
            .and_then(|id| self.store.get(id).cloned())
    }

    pub fn by_value_ref(&self, value_ref: ValueId) -> Expression {
        self.store
            .get(value_ref)
            .cloned()
            .expect("it would be an Anathema compilation error if this failed")
    }

    /// Create a new child and set the new childs id as the `current` id.
    /// Any operations done from here on out are acting upon the new child scope.
    pub fn new_child(&mut self) {
        let parent = self.root.get_scope_mut(&self.current);
        self.current = parent.create_child();
    }

    /// Pop the current child scope, making the current into the parent of
    /// the child.
    ///
    /// E.e if the current id is `[0, 1, 2]` `pop` would result in a new
    /// id of `[0, 1]`.
    pub fn pop(&mut self) {
        self.current = self.current.parent().into();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const VAR_IDENT: StringId = StringId::new(0);

    #[test]
    fn scope_id_next() {
        let id = ScopeId::from([0]);
        assert_eq!(id.next(0).as_slice(), &[0, 0]);
    }

    #[test]
    fn scope_id_parent() {
        let id = ScopeId::from([1, 0]);
        assert_eq!(id.parent(), &[1]);
    }

    #[test]
    fn scope_min() {
        let a = ScopeId::from([1, 0]);
        let b = ScopeId::from([1, 0, 0, 1]);
        let expected = [1, 0].len();
        let actual = a.sub_path_len(&b).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn create_child() {
        let mut root = RootScope::new();
        let child_id = root.create_child();
        assert_eq!(root.0.children.len(), 1);
        assert_eq!(child_id.as_ref(), &[0, 0]);
    }

    #[test]
    fn get_value() {
        let expected: ValueId = 123.into();

        let mut root = RootScope::new();
        root.insert(VAR_IDENT, expected);
        let actual = root.get_value_id(root.id(), VAR_IDENT).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn child_get_value() {
        let expected: ValueId = 1.into();

        let mut root = RootScope::new();
        let child_id = root.create_child();
        let child = root.get_scope_mut(&child_id);
        child.insert(VAR_IDENT, expected);
        let actual = root.get_value_id(&child_id, VAR_IDENT).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn variable_declaration() {
        let mut vars = Variables::new();
        let expected = Variable::from("123");

        vars.declare(VAR_IDENT, expected.clone());
        let value = vars.fetch(VAR_IDENT).unwrap();

        assert_eq!(expected, value);
    }

    #[test]
    fn shadow_value() {
        let mut vars = Variables::new();
        let value_a = Variable::from("1");
        let value_b = Variable::from("2");

        let first_value_ref = vars.declare(VAR_IDENT, value_a.clone());
        let second_value_ref = vars.declare(VAR_IDENT, value_b.clone());
        assert_eq!(value_a, vars.by_value_ref(first_value_ref));
        assert_eq!(value_b, vars.by_value_ref(second_value_ref));
    }

    #[test]
    fn scoping_variables_inaccessible_sibling() {
        // Declare a variable in a sibling and fail to access that value
        let mut vars = Variables::new();

        vars.new_child();
        vars.declare(VAR_IDENT, "inaccessible".into());
        assert!(vars.fetch(VAR_IDENT).is_some());
        vars.pop();

        // Here we should have no access to the value via the root.
        assert!(vars.fetch(VAR_IDENT).is_none());

        // Here we should have no access to the value via the sibling.
        vars.new_child();
        assert!(vars.fetch(VAR_IDENT).is_none());
    }

    #[test]
    fn declaration_lookup() {
        let mut dec = Declarations::new();
        dec.add(VAR_IDENT, [0], 0);
        let root = dec.get_ref(VAR_IDENT, [0, 0]);
        assert_eq!(root, &[0]);
    }

    #[test]
    fn declaration_failed_lookup() {
        let mut dec = Declarations::new();
        dec.add(VAR_IDENT, [0], 0);
        let root = dec.get(VAR_IDENT, [1, 0]);
        assert!(root.is_none());
    }

    #[test]
    fn multi_level_declarations() {
        let mut dec = Declarations::new();
        dec.add(VAR_IDENT, [0], 0);
        dec.add(VAR_IDENT, [0, 0], 0);
        dec.add(VAR_IDENT, [0, 0, 0], 0);

        assert_eq!(dec.get_ref(VAR_IDENT, &[0, 0]), &[0, 0]);
        assert_eq!(dec.get_ref(VAR_IDENT, &[0, 0, 0, 1, 1]), &[0, 0, 0]);
    }

    #[test]
    fn unreachable_declaration() {
        let mut dec = Declarations::new();
        dec.add(VAR_IDENT, [0, 1], 0);
        assert!(dec.get(VAR_IDENT, &[0, 0, 1]).is_none());
    }

    #[test]
    fn assignment_same_scope() {
        let mut vars = Variables::new();
        vars.declare(VAR_IDENT, "1".into());
        vars.assign(VAR_IDENT, "2".into());
        // Declare and assign within the same scope
        assert_eq!(vars.store.count(), 2);
    }
}
