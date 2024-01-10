use anathema_render::Size;
use anathema_values::{
    Attributes, Context, Deferred, DynValue, ExpressionMap, Expressions, Immediate, NextNodeId,
    NodeId, Path, ScopeStorage, State, Value, ValueExpr, ValueRef,
};

pub use self::controlflow::{ElseExpr, IfExpr};
use crate::error::Result;
use crate::factory::FactoryContext;
use crate::nodes::{IfElse, LoopNode, Node, NodeKind, Nodes, Single, View};
use crate::views::{RegisteredViews, Views};
use crate::{Factory, Pos, WidgetContainer};

mod controlflow;

// Create the root view, this is so events can be handled and state can
// be associated with the root view, without having to register additional
// views.
pub fn root_view(body: Vec<Expression>, id: usize) -> Expression {
    Expression::View(ViewExpr {
        id,
        state: None,
        body,
        attributes: Attributes::new(),
    })
}

// -----------------------------------------------------------------------------
//   - A single Node -
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct SingleNodeExpr {
    pub ident: String,
    pub text: Option<ValueExpr>,
    pub attributes: Attributes,
    pub children: Vec<Expression>,
}

impl SingleNodeExpr {
    fn eval<'e>(&'e self, context: &Context<'_, 'e>, node_id: NodeId, scope_storage: ScopeStorage<'e>) -> Result<Node<'e>> {

        let scope = context.new_scope(&scope_storage);
        let context = context.with_scope(&scope);

        let text = self
            .text
            .as_ref()
            .map(|text| String::init_value(&context, &node_id, text))
            .unwrap_or_default();

        let context = FactoryContext::new(
            &context,
            node_id.clone(),
            &self.ident,
            &self.attributes,
            text,
        );

        let widget = WidgetContainer {
            display: context.get("display"),
            background: context.get("background"),
            pos: Pos::ZERO,
            size: Size::ZERO,
            inner: Factory::exec(context)?,
            expr: None,
            attributes: &self.attributes,
        };

        let node = Node {
            kind: NodeKind::Single(Single {
                widget,
                children: Nodes::new(&self.children, node_id.child(0)),
                ident: &self.ident,
            }),
            node_id,
            scope_storage,
        };

        Ok(node)
    }
}

// -----------------------------------------------------------------------------
//   - Loop -
// -----------------------------------------------------------------------------
#[derive(Debug)]
pub(crate) enum Collection<'e> {
    Static(&'e [ValueExpr]),
    State { len: usize, expr: &'e ValueExpr },
    Empty,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body: Vec<Expression>,
    pub binding: Path,
    pub collection: ValueExpr,
}

impl LoopExpr {
    fn eval<'e>(&'e self, context: &Context<'_, 'e>, node_id: NodeId, scope_storage: ScopeStorage<'e>) -> Result<Node<'e>> {
        // Need to know if this is a collection or a path
        let collection = match &self.collection {
            ValueExpr::List(list) => Collection::Static(list),
            col => {
                let mut resolver = Deferred::new(context.lookup());
                let val = col.eval(&mut resolver);
                match val {
                    ValueRef::Expressions(Expressions(list)) => Collection::Static(list),
                    ValueRef::Deferred => {
                        let mut resolver = Immediate::new(context.lookup(), &node_id);
                        let val = col.eval(&mut resolver);
                        let len = match val {
                            ValueRef::List(list) => {
                                // TODO: Review if this makes sense in the long run.
                                //       Right now this is also happening on the update
                                //       for a loop
                                list.subscribe(node_id.clone());
                                list.len()
                            }
                            _ => 0,
                        };

                        Collection::State { expr: col, len }
                    }
                    _ => Collection::Empty,
                }
            }
        };

        let loop_node = LoopNode::new(
            &self.body,
            self.binding.clone(),
            collection,
            node_id.child(0),
        );

        let node = Node {
            kind: NodeKind::Loop(loop_node),
            node_id,
            scope_storage,
        };

        Ok(node)
    }
}

// -----------------------------------------------------------------------------
//   - Controlflow -
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct ControlFlow {
    pub if_expr: IfExpr,
    pub elses: Vec<ElseExpr>,
}

impl ControlFlow {
    fn eval<'e>(&'e self, context: &Context<'_, 'e>, node_id: NodeId, scope_storage: ScopeStorage<'e>) -> Result<Node<'e>> {
        let inner_node_id = node_id.child(0);
        let next_node = NextNodeId::new(node_id.last());

        let node = Node {
            kind: NodeKind::ControlFlow(IfElse::new(
                &self.if_expr,
                &self.elses,
                context,
                inner_node_id,
                next_node,
            )),
            node_id,
            scope_storage,
        };
        Ok(node)
    }
}

pub(crate) enum ViewState<'e> {
    Dynamic(&'e dyn State),
    External { expr: &'e ValueExpr },
    Map(ExpressionMap<'e>),
    Internal,
}

#[derive(Debug, Clone)]
pub struct ViewExpr {
    pub id: usize,
    pub state: Option<ValueExpr>,
    pub body: Vec<Expression>,
    pub attributes: Attributes,
}

impl ViewExpr {
    fn eval<'e>(&'e self, context: &Context<'_, 'e>, node_id: NodeId, scope_storage: ScopeStorage<'e>) -> Result<Node<'e>> {
        let tabindex = self
            .attributes
            .get("tabindex") // TODO: should be a constant. Look into reserving (more) keywords
            .map(|expr| u32::init_value(context, &node_id, expr))
            .unwrap_or(Value::Empty);

        Views::insert(node_id.clone(), tabindex.value());

        let state = match self.state {
            Some(ref expr) => {
                let mut resolver = Deferred::new(context.lookup());
                let val = expr.eval(&mut resolver);
                match val {
                    ValueRef::Map(state) => ViewState::Dynamic(state),
                    ValueRef::Deferred => ViewState::External { expr },
                    ValueRef::ExpressionMap(map) => ViewState::Map(map),
                    _ => ViewState::Internal,
                }
            }
            None => ViewState::Internal,
        };

        let node = Node {
            kind: NodeKind::View(View {
                view: RegisteredViews::get(self.id)?,
                nodes: Nodes::new(&self.body, node_id.child(0)),
                state,
                tabindex,
            }),
            node_id,
            scope_storage,
        };
        Ok(node)
    }
}

// -----------------------------------------------------------------------------
//   - Expression -
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub enum Expression {
    Node(SingleNodeExpr),
    View(ViewExpr),
    Loop(LoopExpr),
    ControlFlow(ControlFlow),
    Declaration(ValueExpr),
    Assignment(ValueExpr),
}

// let outer_scope = {}
// eval(&mut scope) local x = 1 outer_scope[x] = 1
// eval(&mut scope) local y = 2 outer_scope[y] = 2

// eval(vstack_scope) vstack <- gets ownership
//    - new_scope -
//    eval(&mut scope) local x = 1 vstack_scope[x] = 1
//    eval(&mut scope) local y = 2 vstack_scope[y] = 2
//    let text_1_scope = {immutable_ref: vstack_scope}
//    eval(&mut text_1_scope) text x + y // 3
//    eval(&mut scope) local y = 1 vstack_scope[y] = 1
//    text x + y // 2
//
//    let inner_vstack_scope = {immutable_ref: vstack_scope}
//    eval(inner_vstack_scope) vstack
//        text x + y // 2

impl Expression {
    pub(crate) fn eval<'expr>(
        &'expr self,
        context: &Context<'_, 'expr>,
        node_id: NodeId,
        scope_storage: &mut ScopeStorage<'expr>,
    ) -> Result<Node<'expr>> {
        // The scope storage needs to be cloned here, otherwise the values could
        // be incorrect in the future given that there might be new declarations
        // after this expressions.
        //
        // e.g
        //
        // vstack
        //     local x = 1
        //     text x
        //     local x = 2
        //     text x
        //
        // This should output:
        // 1
        // 2
        //
        // However the complete scope storage of the nodes for the vstack
        // would be `x = 2` (as the first one was overwritten by the second one)

        let storage = scope_storage.clone();
        match self {
            Self::Node(node) => node.eval(context, node_id, storage),
            Self::Loop(loop_expr) => loop_expr.eval(context, node_id, storage),
            Self::ControlFlow(controlflow) => controlflow.eval(context, node_id, storage),
            Self::View(view_expr) => view_expr.eval(context, node_id, storage),
            _ => unreachable!()
            // Self::Declaration => {
            //     // modify child_scope
            // }
        }
    }
}

#[cfg(all(test, feature = "testing"))]
mod test {
    use anathema_values::testing::{list, TestState};

    use super::*;
    use crate::contexts::LayoutCtx;
    use crate::layout::Constraints;
    use crate::testing::expressions::{expression, for_expression, if_expression, view_expression};
    use crate::testing::nodes::*;

    impl Expression {
        pub fn test(self) -> TestExpression<TestState> {
            register_test_widget();

            let constraint = Constraints::new(80, 20);

            TestExpression {
                state: TestState::new(),
                expr: Box::new(self),
                layout: LayoutCtx::new(constraint),
            }
        }
    }

    #[derive(Debug)]
    struct AView;
    impl crate::views::View for AView {}

    #[test]
    fn eval_node() {
        let test = expression("test", None, [], []).test();
        let mut node = test.eval().unwrap();
        let (widget, _) = node.single();
        assert_eq!("text", widget.kind());
    }

    #[test]
    fn eval_for() {
        let expr =
            for_expression("item", list([1, 2, 3]), [expression("test", None, [], [])]).test();
        let node = expr.eval().unwrap();

        assert!(matches!(
            node,
            Node {
                kind: NodeKind::Loop { .. },
                ..
            }
        ));
    }

    #[test]
    fn eval_if() {
        let expr = if_expression(
            (true.into(), vec![expression("test", None, [], [])]),
            vec![],
        )
        .test();

        let node = expr.eval().unwrap();

        assert!(matches!(
            node,
            Node {
                kind: NodeKind::ControlFlow(..),
                ..
            }
        ));
    }

    #[test]
    #[should_panic(expected = "ViewNotFound")]
    fn eval_missing_view() {
        let expr = view_expression(12345, None, vec![]).test();
        let _ = expr.eval().unwrap();
    }

    #[test]
    fn eval_prototype_view() {
        RegisteredViews::add_prototype(0, || AView);

        let expr = view_expression(0, None, vec![]).test();
        let node = expr.eval().unwrap();

        assert!(matches!(
            node,
            Node {
                kind: NodeKind::View(..),
                ..
            }
        ));
    }

    #[test]
    #[should_panic(expected = "ViewConsumed")]
    fn consume_view_twice() {
        RegisteredViews::add_view(0, AView);
        let expr = view_expression(0, None, vec![]).test();
        let _ = expr.eval().unwrap();
        let _ = expr.eval().unwrap();
    }
}
