use std::rc::Rc;

use anathema_compiler::Instruction;
use anathema_values::{Attributes, Constants, Expression, StringId, Variables, ViewId, Visibility};
use anathema_widget_core::nodes::{
    ControlFlow, ElseExpr, IfExpr, LoopExpr, Node, SingleNodeExpr, ViewExpr,
};

use crate::const_eval::const_eval;
use crate::error::Result;
use crate::ViewTemplates;

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

    pub fn exec(
        &mut self,
        views: &mut ViewTemplates,
        vars: &mut Variables,
        globals: &mut Variables,
    ) -> Result<Vec<Node>> {
        let mut nodes = vec![];

        if self.instructions.is_empty() {
            return Ok(nodes);
        }

        loop {
            let instruction = self.instructions.remove(0);
            match instruction {
                Instruction::View(ident) => {
                    nodes.push(self.view(ident, views, vars, globals)?);
                }
                Instruction::Node {
                    ident,
                    size: scope_size,
                } => nodes.push(self.node(ident, scope_size, views, vars, globals)?),
                Instruction::For {
                    binding,
                    data,
                    size,
                } => {
                    let binding = self.consts.lookup_string(binding);

                    let collection = self.consts.lookup_value(data).clone();

                    let body = self.instructions.drain(..size).collect();

                    vars.new_child();
                    let body = Scope::new(body, self.consts).exec(views, vars, globals)?;
                    vars.pop();

                    let template = Node::Loop(LoopExpr {
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
                    let body = Scope::new(body, self.consts).exec(views, vars, globals)?;
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
                        let body = Scope::new(body, self.consts).exec(views, vars, globals)?;
                        vars.pop();

                        control_flow.elses.push(ElseExpr {
                            cond,
                            expressions: body,
                        });
                    }

                    let template = Node::ControlFlow(control_flow);
                    nodes.push(template);
                }
                Instruction::Else { .. } => {
                    unreachable!("the `Else` instructions are consumed inside the `If` instruction")
                }
                Instruction::LoadAttribute { .. } | Instruction::LoadValue(_) => {
                    unreachable!("these instructions are only executed in the `node` function")
                }
                Instruction::Declaration {
                    visibility,
                    binding,
                    value,
                } => {
                    let binding: Rc<str> = self.consts.lookup_string(binding).into();
                    let rhs = self.consts.lookup_value(value);
                    let rhs = const_eval(rhs, vars, globals);

                    match visibility {
                        Visibility::Local => vars.declare(binding, rhs),
                        Visibility::Global => globals.declare(binding, rhs),
                    };
                }
            }

            if self.instructions.is_empty() {
                break;
            }
        }

        Ok(nodes)
    }

    fn attributes(&mut self, vars: &Variables, globals: &Variables) -> Attributes {
        let mut attributes = Attributes::new();
        let mut ip = 0;

        while let Some(Instruction::LoadAttribute { key, value }) = self.instructions.get(ip) {
            let key = self.consts.lookup_string(*key);
            let value = self.consts.lookup_value(*value);
            let value = const_eval(value, vars, globals);
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
        globals: &mut Variables,
    ) -> Result<Node> {
        let ident = self.consts.lookup_string(ident);

        let mut text = None::<Expression>;
        let attributes = self.attributes(vars, globals);
        let mut ip = 0;

        while let Some(Instruction::LoadValue(i)) = self.instructions.get(ip) {
            let val = self.consts.lookup_value(*i);
            let val = const_eval(val, vars, globals);
            text = Some(val);
            ip += 1;
        }

        // Remove processed attribute and text instructions
        self.instructions.drain(..ip);

        let scope = self.instructions.drain(..scope_size).collect();
        vars.new_child();
        let children = Scope::new(scope, self.consts).exec(views, vars, globals)?;
        vars.pop();

        let node = Node::Single(SingleNodeExpr {
            ident: ident.to_string(),
            text,
            attributes,
            children,
        });

        Ok(node)
    }

    fn view(
        &mut self,
        view: ViewId,
        views: &mut ViewTemplates,
        vars: &Variables,
        globals: &mut Variables,
    ) -> Result<Node> {
        let attributes = self.attributes(vars, globals);

        let state = match self.instructions.first() {
            Some(Instruction::LoadValue(i)) => {
                let val = self.consts.lookup_value(*i).clone();
                let val = const_eval(val, vars, globals);
                let _ = self.instructions.remove(0);
                Some(val)
            }
            _ => None,
        };

        let body = views.get(view, globals)?;

        let node = Node::View(ViewExpr {
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
    use anathema_values::testing::{add, dot, ident, map, unum};

    use super::*;
    use crate::testing::TestScope;

    #[test]
    fn scope_access() {
        // local a = 1
        // vstack
        //    local a = 2
        //    text a // this should output 2
        let mut test_scope = TestScope::new();
        test_scope.local("a", unum(1));
        test_scope.node("vstack", 3);
        test_scope.local("a", unum(2));
        test_scope.node("text", 0);
        test_scope.load_value(ident("a"));
        let actual = test_scope.exec();
        panic!("{actual:#?}");
        // assert_eq!(expected, actual);
    }

    #[test]
    fn eval_instruction() {
        // let mut test_scope = TestScope::new();
        // // test_scope.node("ident here", 0);
        // // test_scope.for_loop("i", [1, 2], 0);
        // // test_scope.attrib("i", "y");
        // // test_scope.local("a", add(modulo(unum(8), unum(3)), ident("lol")));
        // // test_scope.local("b", add(ident("a"), unum(2)));
        // // test_scope.local("c", list([
        // //         add(ident("a"), unum(2)),
        // //         add(ident("b"), unum(2)),
        // //         add(ident("doesnotexist"), unum(1)),
        // // ]));
        // // test_scope.local("d", map([("key", add(ident("a"), ident("b")))]));

        // // test_scope.local("c", unum(0));
        // // test_scope.assign(ident("c"), unum(0));

        // test_scope.local("a", map([("b", map([("c", unum(1))]))]));

        // let assign = dot(dot(ident("a"), ident("b")), ident("c"));
        // test_scope.assign(assign.clone(), unum(999));
        // test_scope.local("xx", add(assign, unum(1)));
        // test_scope.local("y", ident("xx"));

        // let expr = test_scope.exec().unwrap();
        // panic!("{:#?}", test_scope.vars);
    }
}
