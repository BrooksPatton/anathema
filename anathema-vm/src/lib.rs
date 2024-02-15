mod const_eval;
mod error;
mod scope;
mod testing;
mod vm;

use anathema_values::hashmap::HashMap;
use anathema_values::{ViewId, ViewIds, Variables};
use anathema_widget_core::nodes::{root_view, Node};
use anathema_widget_core::views::{AnyView, RegisteredViews, View};
pub use vm::VirtualMachine;

use self::error::{Error, Result};

#[derive(Debug)]
struct ViewTemplates {
    view_ids: ViewIds,
    inner: HashMap<ViewId, Template>,
    dep_list: Vec<ViewId>,
}

impl ViewTemplates {
    fn new() -> Self {
        Self {
            view_ids: ViewIds::new(),
            inner: HashMap::new(),
            dep_list: vec![],
        }
    }

    fn get(&mut self, view: ViewId, globals: &mut Variables) -> Result<Vec<Node>> {
        if self.dep_list.iter().any(|v| view.eq(v)) {
            panic!("circular dependencies");
        }

        self.dep_list.push(view);

        let ret = match self.inner.remove(&view) {
            None => return Err(Error::TemplateMissing),
            Some(Template::Pending(src)) => {
                let expressions = templates(&src, self, globals)?;
                self.inner
                    .insert(view, Template::Evaluated(expressions.clone()));
                Ok(expressions)
            }
            Some(Template::Evaluated(expressions)) => {
                let e = expressions.clone();
                self.inner.insert(view, Template::Evaluated(expressions));
                Ok(e)
            }
        };

        self.dep_list.pop();

        ret
    }

    fn insert(&mut self, view: String, template: String) -> ViewId {
        let view = self.view_ids.push(view);
        self.inner.insert(view, Template::Pending(template));
        view
    }
}

#[derive(Debug)]
pub struct Templates {
    root: String,
    view_templates: ViewTemplates,
}

impl Templates {
    pub fn new(root: String, view: impl View + Send + 'static) -> Self {
        let view_templates = ViewTemplates::new();
        RegisteredViews::add_view(view_templates.view_ids.root_id(), view);
        Self {
            root,
            view_templates,
        }
    }

    pub fn compile(&mut self) -> Result<CompiledTemplates> {
        let mut globals = Variables::new();
        let expressions = templates(&self.root, &mut self.view_templates, &mut globals)?;
        let root = root_view(expressions, self.view_templates.view_ids.root_id());
        Ok(CompiledTemplates { root: vec![root] })
    }

    pub fn add_view(
        &mut self,
        ident: impl Into<String>,
        template: String,
        view: impl AnyView + 'static,
    ) -> ViewId {
        let ident = ident.into();
        let view_id = self.view_templates.insert(ident.clone(), template);
        RegisteredViews::add_view(view_id, view);
        view_id
    }

    pub fn add_prototype<F, T>(&mut self, ident: impl Into<String>, template: String, f: F)
    where
        F: Send + 'static + FnMut() -> T,
        T: 'static + View + Send,
    {
        let ident = ident.into();
        let view_id = self.view_templates.insert(ident.clone(), template);
        RegisteredViews::add_prototype(view_id, f)
    }
}

#[derive(Debug)]
pub struct CompiledTemplates {
    root: Vec<Node>,
}

impl CompiledTemplates {
    pub fn expressions(&self) -> &[Node] {
        &self.root
    }
}

#[derive(Debug)]
enum Template {
    Pending(String),
    Evaluated(Vec<Node>),
}

fn templates(root: &str, views: &mut ViewTemplates, globals: &mut Variables) -> Result<Vec<Node>> {
    let (instructions, constants) = anathema_compiler::compile(root, &mut views.view_ids)?;
    let vm = VirtualMachine::new(instructions, constants);
    vm.exec(views, globals)
}

#[cfg(test)]
mod test {
    use super::*;

    struct AView;
    impl View for AView {}

    #[test]
    #[should_panic(expected = "circular dependencies")]
    fn circular_deps() {
        let mut t = Templates::new("@a".into(), ());
        t.add_view("a", "@b".to_string(), AView);
        t.add_view("b", "@a".to_string(), AView);
        t.compile().unwrap();
    }
}
