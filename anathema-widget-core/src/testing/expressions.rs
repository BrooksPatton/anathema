use anathema_values::{Attributes, Path, Expression};

use crate::nodes::{
    ControlFlow, ElseExpr, Node, IfExpr, LoopExpr, SingleNodeExpr, ViewExpr,
};

pub fn expression(
    ident: impl Into<String>,
    text: impl Into<Option<Expression>>,
    attributes: impl IntoIterator<Item = (String, Expression)>,
    children: impl Into<Vec<Node>>,
) -> Node {
    let children = children.into();
    Node::Single(SingleNodeExpr {
        ident: ident.into(),
        text: text.into(),
        attributes: Attributes::from_iter(attributes),
        children,
    })
}

#[allow(clippy::boxed_local)]
pub fn for_expression<'e>(
    binding: impl Into<String>,
    collection: Box<Expression>,
    body: impl Into<Vec<Node>>,
) -> Node {
    Node::Loop(LoopExpr {
        body: body.into(),
        binding: binding.into(),
        collection: *collection,
    })
}

pub fn if_expression(
    if_true: (Expression, Vec<Node>),
    elses: Vec<(Option<Expression>, Vec<Node>)>,
) -> Node {
    Node::ControlFlow(ControlFlow {
        if_expr: IfExpr {
            cond: if_true.0,
            expressions: if_true.1,
        },
        elses: elses
            .into_iter()
            .map(|(cond, body)| ElseExpr {
                cond,
                expressions: body,
            })
            .collect(),
    })
}

pub fn view_expression(id: usize, state: Option<Expression>, body: Vec<Node>) -> Node {
    Node::View(ViewExpr {
        id,
        state,
        body,
        attributes: Attributes::new(),
    })
}
