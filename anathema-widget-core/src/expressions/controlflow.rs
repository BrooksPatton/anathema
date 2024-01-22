use anathema_values::ExpressionBanana;

use super::Node;

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: ExpressionBanana,
    pub expressions: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct ElseExpr {
    pub cond: Option<ExpressionBanana>,
    pub expressions: Vec<Node>,
}
