use anathema_store::storage::strings::{StringId, Strings};

use crate::blueprints::Blueprint;
use crate::components::{ComponentId, ComponentTemplates};
use crate::error::Result;
use crate::expressions::Expression;
use crate::variables::{Variables, Visibility};

mod const_eval;
pub(crate) mod eval;
pub(super) mod parser;

pub(crate) struct Context<'vars, 'strings> {
    pub(crate) locals: &'vars mut Variables,
    pub(crate) globals: &'vars mut Variables,
    pub(crate) components: &'strings mut ComponentTemplates,
    pub(crate) strings: &'strings Strings,
}

impl Context<'_, '_> {
    fn fetch(&self, key: &str) -> Option<Expression> {
        self.locals.fetch(key).or_else(|| self.globals.fetch(key))
    }

    fn load_component(&mut self, component_id: ComponentId, locals: Variables) -> Result<Vec<Blueprint>> {
        self.components.load(component_id, locals, self.globals)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Statement {
    LoadValue(Expression),
    LoadAttribute {
        key: StringId,
        value: Expression,
    },
    Component(ComponentId),
    Node(StringId),
    For {
        binding: StringId,
        data: Expression,
    },
    Declaration {
        visibility: Visibility,
        binding: StringId,
        value: Expression,
    },
    If(Expression),
    Else(Option<Expression>),
    ScopeStart,
    ScopeEnd,
    Eof,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Statements(Vec<Statement>);

impl From<Vec<Statement>> for Statements {
    fn from(value: Vec<Statement>) -> Self {
        Self(value)
    }
}

impl FromIterator<Statement> for Statements {
    fn from_iter<T: IntoIterator<Item = Statement>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Statements {
    fn next(&mut self) -> Option<Statement> {
        match self.is_empty() {
            false => Some(self.0.remove(0)),
            true => None,
        }
    }

    fn take_value(&mut self) -> Option<Expression> {
        match matches!(&self.0.first(), Some(Statement::LoadValue(_))) {
            true => match self.0.remove(0) {
                Statement::LoadValue(expr) => Some(expr),
                _ => unreachable!(),
            },
            false => None,
        }
    }

    fn take_attributes(&mut self) -> Vec<(StringId, Expression)> {
        let mut v = vec![];
        while matches!(&self.0.first(), Some(Statement::LoadAttribute { .. })) {
            match self.0.remove(0) {
                Statement::LoadAttribute { key, value } => v.push((key, value)),
                _ => unreachable!(),
            }
        }
        v
    }

    fn take_scope(&mut self) -> Statements {
        if self.is_empty() {
            return vec![].into();
        }

        if self.0[0] != Statement::ScopeStart {
            return vec![].into();
        }

        let mut level = 0;

        for i in 0..self.0.len() {
            match &self.0[i] {
                Statement::ScopeStart => level += 1,
                Statement::ScopeEnd if level - 1 == 0 => {
                    let mut scope = self.0.split_off(i);
                    scope.remove(0); // remove the scope start
                    std::mem::swap(&mut scope, &mut self.0);
                    scope.remove(0); // remove the scope end
                    return scope.into();
                }
                Statement::ScopeEnd => level -= 1,
                _ => continue,
            }
        }

        panic!("unclosed scope");
    }

    fn next_else(&mut self) -> Option<Option<Expression>> {
        match matches!(self.0.first(), Some(Statement::Else(_))) {
            true => match self.0.remove(0) {
                Statement::Else(cond) => Some(cond),
                _ => unreachable!(),
            },
            false => None,
        }
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// This is useful for testing
#[cfg(test)]
fn with_context<F>(mut f: F)
where
    F: FnMut(Context<'_, '_>),
{
    let mut locals = Variables::new();
    let mut globals = Variables::new();
    let strings = Strings::empty();
    let mut components = ComponentTemplates::new();

    let context = Context {
        locals: &mut locals,
        globals: &mut globals,
        strings: &strings,
        components: &mut components,
    };

    f(context)
}

// -----------------------------------------------------------------------------
//   - Statements -
// -----------------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::*;

    pub(crate) fn load_value(expr: impl Into<Expression>) -> Statement {
        Statement::LoadValue(expr.into())
    }

    pub(crate) fn load_attrib(key: impl Into<StringId>, expr: impl Into<Expression>) -> Statement {
        Statement::LoadAttribute {
            key: key.into(),
            value: expr.into(),
        }
    }

    pub(crate) fn view(id: impl Into<ComponentId>) -> Statement {
        Statement::Component(id.into())
    }

    pub(crate) fn node(id: impl Into<StringId>) -> Statement {
        Statement::Node(id.into())
    }

    pub(crate) fn for_loop(binding: impl Into<StringId>, data: impl Into<Expression>) -> Statement {
        Statement::For {
            binding: binding.into(),
            data: data.into(),
        }
    }

    pub(crate) fn global_decl(binding: impl Into<StringId>, value: impl Into<Expression>) -> Statement {
        Statement::Declaration {
            visibility: Visibility::Global,
            binding: binding.into(),
            value: value.into(),
        }
    }

    pub(crate) fn local_decl(binding: impl Into<StringId>, value: impl Into<Expression>) -> Statement {
        Statement::Declaration {
            visibility: Visibility::Local,
            binding: binding.into(),
            value: value.into(),
        }
    }

    pub(crate) fn if_stmt(cond: impl Into<Expression>) -> Statement {
        Statement::If(cond.into())
    }

    pub(crate) fn if_else(cond: impl Into<Expression>) -> Statement {
        Statement::Else(Some(cond.into()))
    }

    pub(crate) fn else_stmt() -> Statement {
        Statement::Else(None)
    }

    pub(crate) fn scope_start() -> Statement {
        Statement::ScopeStart
    }

    pub(crate) fn scope_end() -> Statement {
        Statement::ScopeEnd
    }

    pub(crate) fn eof() -> Statement {
        Statement::Eof
    }

    #[test]
    pub(crate) fn split_scope() {
        let mut statements = Statements::from_iter([scope_start(), node(0), scope_end(), node(1)]);
        let scope = statements.take_scope();
        assert_eq!(Statements::from_iter([node(0)]), scope);
        assert_eq!(Statements::from_iter([node(1)]), statements);
    }
}