use anathema_values::ExpressionBanana;

use super::Expression;

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: ExpressionBanana,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ElseExpr {
    pub cond: Option<ExpressionBanana>,
    pub expressions: Vec<Expression>,
}
