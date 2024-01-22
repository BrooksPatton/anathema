use std::rc::Rc;

use anathema_compiler::{Constants, Instruction, StringId, ViewId};
use anathema_values::hashmap::HashMap;
use anathema_values::{Attributes, ValueExpr, Visibility};
use anathema_widget_core::expressions::{
    ControlFlow, ElseExpr, Expression, IfExpr, LoopExpr, SingleNodeExpr, ViewExpr,
};

use crate::error::Result;
use crate::ViewTemplates;
use crate::variables::Variables;

pub(crate) struct Scope<'vm> {
    instructions: Vec<Instruction>,
    consts: &'vm Constants,
}

impl<'vm> Scope<'vm> {
    pub fn new(instructions: Vec<Instruction>, consts: &'vm Constants) -> Self {
        Self {
            instructions,
            consts,
        }
    }

    pub fn exec(&mut self, views: &mut ViewTemplates, vars: &mut Variables) -> Result<Vec<Expression>> {
        let mut nodes = vec![];

        if self.instructions.is_empty() {
            return Ok(nodes);
        }

        loop {
            let instruction = self.instructions.remove(0);
            match instruction {
                Instruction::View(ident) => {
                    nodes.push(self.view(ident, views)?);
                }
                Instruction::Node { ident, size: scope_size } => {
                    nodes.push(self.node(ident, scope_size, views, vars)?)
                }
                Instruction::For {
                    binding,
                    data,
                    size,
                } => {
                    let binding = self.consts.lookup_string(binding);

                    let collection = self.consts.lookup_value(data).clone();

                    let body = self.instructions.drain(..size).collect();

                    vars.new_child();
                    let body = Scope::new(body, self.consts).exec(views, vars)?;
                    vars.pop();

                    let template = Expression::Loop(LoopExpr {
                        binding: binding.into(),
                        collection,
                        body,
                    });

                    nodes.push(template);
                }
                Instruction::If { cond, size } => {
                    let cond = self.consts.lookup_value(cond);

                    let body = self.instructions.drain(..size).collect::<Vec<_>>();
                    vars.new_child();
                    let body = Scope::new(body, self.consts).exec(views, vars)?;
                    vars.pop();

                    let mut control_flow = ControlFlow {
                        if_expr: IfExpr {
                            cond,
                            expressions: body,
                        },
                        elses: vec![],
                    };

                    loop {
                        let Some(&Instruction::Else { cond, size }) = self.instructions.first()
                        else {
                            break;
                        };
                        self.instructions.remove(0);
                        let cond = cond.map(|cond| self.consts.lookup_value(cond));

                        let body = self.instructions.drain(..size).collect();
                        vars.new_child();
                        let body = Scope::new(body, self.consts).exec(views, vars)?;
                        vars.pop();

                        control_flow.elses.push(ElseExpr {
                            cond,
                            expressions: body,
                        });
                    }

                    let template = Expression::ControlFlow(control_flow);
                    nodes.push(template);
                }
                Instruction::Else { .. } => {
                    unreachable!("the `Else` instructions are consumed inside the `If` instruction")
                }
                Instruction::LoadAttribute { .. } | Instruction::LoadValue(_) => {
                    unreachable!("these instructions are only executed in the `node` function")
                }
                Instruction::Declaration { visibility, binding, value } => {
                    let value = self.consts.lookup_value(value);
                    let binding = self.consts.lookup_string(binding);
                    // insert the declaration into the var table
                    // let expr = Expression::Assignment { };
                    let expr = panic!();
                    nodes.push(expr);
                }
                Instruction::Assignment { .. } => todo!(),
                // Instruction::Assignment(value_id) => {
                //     let assignment = self.consts.lookup_value(value_id);
                //     let expr = Expression::Assignment(assignment);
                //     nodes.push(expr);
                // }
            }

            if self.instructions.is_empty() {
                break;
            }
        }

        Ok(nodes)
    }

    fn attributes(&mut self) -> Attributes {
        let mut attributes = Attributes::new();
        let mut ip = 0;

        while let Some(Instruction::LoadAttribute { key, value }) = self.instructions.get(ip) {
            let key = self.consts.lookup_string(*key);
            let value = self.consts.lookup_value(*value);
            attributes.insert(key.to_string(), value.clone());
            ip += 1;
        }

        // Remove processed attribute and text instructions
        self.instructions.drain(..ip);
        attributes
    }

    fn node(
        &mut self,
        ident: StringId,
        scope_size: usize,
        views: &mut ViewTemplates,
        vars: &mut Variables,
    ) -> Result<Expression> {
        let ident = self.consts.lookup_string(ident);

        let mut text = None::<ValueExpr>;
        // let mut attributes = Attributes::new();
        let attributes = self.attributes();
        let mut ip = 0;

        while let Some(Instruction::LoadValue(i)) = self.instructions.get(ip) {
            text = Some(self.consts.lookup_value(*i).clone());
            ip += 1;
        }

        // Remove processed attribute and text instructions
        self.instructions.drain(..ip);

        let scope = self.instructions.drain(..scope_size).collect();
        vars.new_child();
        let children = Scope::new(scope, self.consts).exec(views, vars)?;
        vars.pop();

        let node = Expression::Node(SingleNodeExpr {
            ident: ident.to_string(),
            text,
            attributes,
            children,
        });

        Ok(node)
    }

    fn view(&mut self, view: ViewId, views: &mut ViewTemplates) -> Result<Expression> {
        let attributes = self.attributes();

        let state = match self.instructions.first() {
            Some(Instruction::LoadValue(i)) => {
                let val = self.consts.lookup_value(*i).clone();
                let _ = self.instructions.remove(0);
                Some(val)
            }
            _ => None,
        };

        let body = views.get(view)?;

        let node = Expression::View(ViewExpr {
            id: view.0,
            body,
            attributes,
            state,
        });

        Ok(node)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestScope {
        consts: Constants,
        vars: Variables,
        views: ViewTemplates,
        instructions: Vec<Instruction>,
    }

    impl TestScope {
        pub fn new() -> Self {
            Self {
                consts: Constants::new(),
                views: ViewTemplates::new(),
                vars: Variables::new(),
                instructions: vec![],
            }
        }

        fn exec(mut self) -> Result<Box<[Expression]>> {
            let Self {
                consts,
                mut views,
                mut vars,
                instructions,
            } = self;

            let mut scope = Scope::new(instructions, &consts);
            scope.exec(&mut views, &mut vars).map(Vec::into_boxed_slice)
        }

        fn node(&mut self, ident: &str, scope_size: usize) {
            let ident = self.consts.store_string(ident);
            let inst = Instruction::Node {
                ident,
                size: scope_size,
            };
            self.instructions.push(inst);
        }

        fn for_loop(&mut self, binding: &str, data: impl Into<ValueExpr>, size: usize) {
            let binding = self.consts.store_string(binding);
            let data = self.consts.store_value(data.into());
            let inst = Instruction::For {
                binding,
                data,
                size,
            };
            self.instructions.push(inst);
        }

        fn if_stmt(&mut self, cond: impl Into<ValueExpr>, size: usize) {
            let cond = self.consts.store_value(cond.into());
            let inst = Instruction::If {
                cond,
                size,
            };
            self.instructions.push(inst);
        }

        fn else_stmt(&mut self, cond: Option<impl Into<ValueExpr>>, size: usize) {
            let cond = cond.map(|cond| self.consts.store_value(cond.into()));
            let inst = Instruction::Else {
                cond,
                size,
            };
            self.instructions.push(inst);
        }

        fn attrib(&mut self, key: &str, value: impl Into<ValueExpr>) {
            let key = self.consts.store_string(key);
            let value = self.consts.store_value(value.into());
            let inst = Instruction::LoadAttribute {
                key,
                value,
            };
            self.instructions.push(inst);
        }

        fn decl(&mut self, visibility: Visibility, binding: &str, value: impl Into<ValueExpr>) {
            let binding = self.consts.store_string(binding);
            let value = self.consts.store_value(value.into());
            let inst = Instruction::Declaration {
                visibility,
                binding,
                value,
            };
            self.instructions.push(inst);
        }

        fn local(&mut self, binding: &str, value: impl Into<ValueExpr>) {
            self.decl(Visibility::Local, binding, value)
        }

        fn global(&mut self, binding: &str, value: impl Into<ValueExpr>) {
            self.decl(Visibility::Global, binding, value)
        }
    }

    #[test]
    fn eval_instruction() {
        let mut test_scope = TestScope::new();
        // test_scope.node("ident here", 0);
        // test_scope.for_loop("i", [1, 2], 0);
        // test_scope.attrib("i", "y");
        test_scope.local("aa", "bb");
        let expr = test_scope.exec().unwrap();
        panic!("{expr:#?}");
    }
}
