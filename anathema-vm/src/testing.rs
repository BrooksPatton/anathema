use anathema_compiler::Instruction;
use anathema_values::{Constants, Expression, Variables, Visibility};
use anathema_widget_core::nodes::Node;

use crate::error::Result;
use crate::scope::Scope;
use crate::ViewTemplates;

#[derive(Debug)]
pub struct TestScope {
    pub consts: Constants,
    pub vars: Variables,
    pub globals: Variables,
    pub views: ViewTemplates,
    pub instructions: Vec<Instruction>,
}

impl TestScope {
    pub fn new() -> Self {
        Self {
            consts: Constants::new(),
            views: ViewTemplates::new(),
            vars: Variables::new(),
            globals: Variables::new(),
            instructions: vec![],
        }
    }

    pub fn exec(&mut self) -> Result<Box<[Node]>> {
        let mut scope = Scope::new(self.instructions.drain(..).collect(), &self.consts);

        let output = scope
            .exec(&mut self.views, &mut self.vars, &mut self.globals)
            .map(Vec::into_boxed_slice);

        output
    }

    pub fn node(&mut self, ident: &str, scope_size: usize) {
        let ident = self.consts.store_string(ident);
        let inst = Instruction::Node {
            ident,
            size: scope_size,
        };
        self.instructions.push(inst);
    }

    pub fn for_loop(&mut self, binding: &str, data: impl Into<Expression>, size: usize) {
        let binding = self.consts.store_string(binding);
        let data = self.consts.store_value(data.into());
        let inst = Instruction::For {
            binding,
            data,
            size,
        };
        self.instructions.push(inst);
    }

    pub fn if_stmt(&mut self, cond: impl Into<Expression>, size: usize) {
        let cond = self.consts.store_value(cond.into());
        let inst = Instruction::If { cond, size };
        self.instructions.push(inst);
    }

    pub fn else_stmt(&mut self, cond: Option<impl Into<Expression>>, size: usize) {
        let cond = cond.map(|cond| self.consts.store_value(cond.into()));
        let inst = Instruction::Else { cond, size };
        self.instructions.push(inst);
    }

    pub fn attrib(&mut self, key: &str, value: impl Into<Expression>) {
        let key = self.consts.store_string(key);
        let value = self.consts.store_value(value.into());
        let inst = Instruction::LoadAttribute { key, value };
        self.instructions.push(inst);
    }

    pub fn load_value(&mut self, value: impl Into<Expression>) {
        let value = self.consts.store_value(value.into());
        let inst = Instruction::LoadValue(value);
        self.instructions.push(inst);
    }

    pub fn decl(&mut self, visibility: Visibility, binding: &str, value: impl Into<Expression>) {
        let binding = self.consts.store_string(binding);
        let value = self.consts.store_value(value.into());
        let inst = Instruction::Declaration {
            visibility,
            binding,
            value,
        };
        self.instructions.push(inst);
    }

    pub fn local(&mut self, binding: &str, value: impl Into<Expression>) {
        self.decl(Visibility::Local, binding, value)
    }

    pub fn global(&mut self, binding: &str, value: impl Into<Expression>) {
        self.decl(Visibility::Global, binding, value)
    }
}
