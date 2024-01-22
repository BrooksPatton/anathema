use std::ops::ControlFlow;

use anathema_values::{
    Change, Context, Deferred, NextNodeId, NodeId, Path, Scope, ScopeValue, ValueRef,
};

use super::Nodes;
use crate::error::Result;
use crate::expressions::{Collection, Expression};
use crate::WidgetContainer;

/// An iteration inside a loop node.
#[derive(Debug)]
pub(in crate::nodes) struct Iteration<'e> {
    pub(super) body: Nodes<'e>,
    node_id: NodeId,
    loop_index: usize,
    loop_value: ScopeValue<'e>,
}

// -----------------------------------------------------------------------------
//   - Loop -
// -----------------------------------------------------------------------------
#[derive(Debug)]
pub struct LoopNode<'e> {
    expressions: &'e [Expression],
    pub(super) iterations: Vec<Iteration<'e>>,
    current_iteration: usize,
    pub(super) binding: &'e str,
    pub(super) collection: Collection<'e>,
    pub(super) value_index: usize,
    node_id: NodeId,
    next_node_id: NextNodeId,
}

impl<'e> LoopNode<'e> {
    pub(crate) fn new(
        expressions: &'e [Expression],
        binding: &'e str,
        collection: Collection<'e>,
        node_id: NodeId,
    ) -> Self {
        let next_node_id = NextNodeId::new(node_id.last());
        Self {
            expressions,
            iterations: vec![],
            binding,
            collection,
            value_index: 0,
            current_iteration: 0,
            node_id,
            next_node_id,
        }
    }

    pub(super) fn next<F>(
        &mut self,
        context: &Context<'_, 'e>,
        f: &mut F,
    ) -> Result<ControlFlow<(), ()>>
    where
        F: FnMut(&mut WidgetContainer<'e>, &mut Nodes<'e>, &Context<'_, 'e>) -> Result<()>,
    {
        loop {
            let Some(scope_val) = self.scope_next_value(context) else {
                return Ok(ControlFlow::Continue(()));
            };

            let loop_index = self.value_index;

            self.value_index += 1;

            let iter = match self.iterations.get_mut(self.current_iteration) {
                Some(iter) => iter,
                None => {
                    let node_id = self.next_node_id.next(&self.node_id);

                    self.iterations.push(Iteration {
                        body: Nodes::new(self.expressions, node_id.child(0)),
                        node_id,
                        loop_index,
                        loop_value: scope_val,
                    });
                    &mut self.iterations[self.current_iteration]
                }
            };

            let mut inner = context.inner();
            let scopes = inner.scope("loop", ScopeValue::value(iter.loop_index));
            let scopes = scopes.scope_value(self.binding, iter.loop_value);
            inner.assign(&scopes);
            let context = inner.into();

            loop {
                let res = iter.body.next(&context, f)?;
                match res {
                    ControlFlow::Continue(()) => continue,
                    ControlFlow::Break(()) => break,
                }
            }
            self.current_iteration += 1;
        }
    }

    pub(super) fn reset_cache(&mut self) {
        self.current_iteration = 0;
        self.value_index = 0;
        self.iterations
            .iter_mut()
            .for_each(|i| i.body.reset_cache());
    }

    pub(super) fn count(&self) -> usize {
        self.iterations.iter().map(|i| i.body.count()).sum()
    }

    fn scope_next_value(&mut self, context: &Context<'_, 'e>) -> Option<ScopeValue<'e>> {
        match self.collection {
            Collection::Static(expressions) => {
                let expr = expressions.get(self.value_index)?;
                let mut resolver = Deferred::new(context.lookup());
                let val = match expr.eval(&mut resolver) {
                    ValueRef::Deferred => ScopeValue::Deferred(expr),
                    value => ScopeValue::Value(value),
                };
                Some(val)
            }
            Collection::State { len, .. } if len == self.value_index => None,
            Collection::State { expr, .. } => {
                Some(ScopeValue::DeferredList(self.value_index, expr))
            }
            Collection::Empty => None,
        }
    }

    // TODO: this is not the most optimal solution.
    //       and it's leaving a bit of performance on the tabel.
    //       Review this at some stage
    pub(super) fn smush(&mut self) {
        self.iterations.clear();
        self.reset_cache();
    }

    pub(super) fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (&mut WidgetContainer<'e>, &mut Nodes<'e>)> + '_ {
        self.iterations.iter_mut().flat_map(|i| i.body.iter_mut())
    }

    pub(super) fn update(&mut self, node_id: &[usize], change: &Change, context: &Context<'_, '_>) {
        for iter in &mut self.iterations {
            let mut inner_context = context.inner();
            let mut scopes = inner_context.scope(
                "loop",
                ScopeValue::Value(ValueRef::Owned(iter.loop_index.into())),
            );
            scopes.scope_value(self.binding, iter.loop_value);
            inner_context.assign(&scopes);
            let context = inner_context.into();

            if iter.node_id.contains(node_id) {
                iter.body.update(node_id, change, &context);
                break;
            }
        }
    }
}

#[cfg(test)]
mod test {
    // use anathema_render::Size;
    // use anathema_values::testing::{dot, ident, TestState};
    // use anathema_values::{drain_dirty_nodes, ValueExpr};

    // use super::*;
    // use crate::generator::testing::{register_test_widget, TestLayoutMany, TestWidget};
    // use crate::layout::{Constraints, Layouts};
    // use crate::testing::{expression, for_expression};
    // use crate::Padding;

    // fn expr() -> Vec<Expression> {
    //     let expression = for_expression(
    //         "name",
    //         ident("generic_list"),
    //         vec![expression("test", Some(*ident("name")), [], [])],
    //     );
    //     vec![expression]
    // }

    // struct LoopNodeTest<'e> {
    //     expressions: &'e [Expression],
    //     state: TestState,
    //     nodes: Nodes<'e>,
    // }

    // impl<'e> LoopNodeTest<'e> {
    //     fn new(expressions: &'e [Expression]) -> Self {
    //         register_test_widget();
    //         let nodes = Nodes::new(&expressions, 0.into());
    //         Self {
    //             state: TestState::new(),
    //             expressions,
    //             nodes,
    //         }
    //     }

    //     fn notify(&mut self) {
    //         let dirt = drain_dirty_nodes();
    //         let context = Context::root(&self.state);
    //         for (id, change) in dirt {
    //             self.nodes.update(id.as_slice(), &change, &context);
    //         }
    //     }

    //     fn names(&mut self) -> Vec<String> {
    //         self.nodes
    //             .iter_mut()
    //             .map(|(n, _)| n.to_ref::<TestWidget>())
    //             .map(|widget| widget.0.str().to_string())
    //             .collect()
    //     }

    //     fn count(&self) -> usize {
    //         self.nodes.count()
    //     }

    //     fn layout(&mut self) -> Result<Size> {
    //         self.nodes.reset_cache();
    //         let context = Context::root(&self.state);
    //         let layout = LayoutCtx::new(Constraints::new(100, 100), Padding::ZERO);
    //         let mut layout = Layouts::new(TestLayoutMany, &layout);
    //         layout.layout(&mut self.nodes, &context)
    //     }
    // }

    // #[test]
    // fn remove_node() {
    //     let expr = expr();
    //     let mut test = LoopNodeTest::new(&expr);
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2", "3"]);
    //     test.state.generic_list.remove(1);

    //     test.notify();
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "3"]);
    // }

    // #[test]
    // fn pop() {
    //     let expr = expr();
    //     let mut test = LoopNodeTest::new(&expr);
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2", "3"]);

    //     test.state.generic_list.pop();
    //     test.notify();
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2"]);
    // }

    // #[test]
    // fn push_node() {
    //     let expr = expr();
    //     let mut test = LoopNodeTest::new(&expr);
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2", "3"]);

    //     test.state.generic_list.push(100);
    //     test.notify();
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2", "3", "100"]);
    // }

    // #[test]
    // fn insert_node() {
    //     let expr = expr();
    //     let mut test = LoopNodeTest::new(&expr);
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "2", "3"]);

    //     test.state.generic_list.insert(1, 100);
    //     test.notify();
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["1", "100", "2", "3"]);

    //     test.state.generic_list.insert(0, 99);
    //     test.notify();
    //     test.layout().unwrap();
    //     let n = test.names();
    //     assert_eq!(&n, &["99", "1", "100", "2", "3"]);
    // }

    // #[test]
    // fn nested_next_value() {
    //     let state = TestState::new();
    //     let context = Context::root(&state);
    //     let outer = for_expression(
    //         "outer",
    //         ValueExpr::Ident("nested_list".into()).into(),
    //         vec![for_expression(
    //             "inner",
    //             ValueExpr::Ident("outer".into()).into(),
    //             vec![expression("test", ValueExpr::Ident("inner".into()), [], [])],
    //         )],
    //     );

    //     let xx = outer.eval(&context, 0.into()).unwrap();

    //     let y = xx;
    // }
}
