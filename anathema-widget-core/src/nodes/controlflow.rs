use anathema_values::Expression;

use super::Node;

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Expression,
    pub expressions: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct ElseExpr {
    pub cond: Option<Expression>,
    pub expressions: Vec<Node>,
}
