use anathema_compiler::Instruction;
use anathema_values::Constants;
use anathema_widget_core::expressions::Expression;

use crate::error::Result;
use crate::scope::Scope;
use crate::variables::Variables;
use crate::ViewTemplates;

pub struct VirtualMachine {
    instructions: Vec<Instruction>,
    consts: Constants,
}

impl VirtualMachine {
    pub fn new(instructions: Vec<Instruction>, consts: Constants) -> Self {
        Self {
            instructions,
            consts,
        }
    }

    pub(super) fn exec(self, views: &mut ViewTemplates) -> Result<Vec<Expression>> {
        let mut root_scope = Scope::new(self.instructions, &self.consts);
        let mut vars = Variables::new();
        root_scope.exec(views, &mut vars)
    }
}

#[cfg(test)]
mod test {
    use anathema_compiler::compile;
    use anathema_values::ViewIds;
    use anathema_widget_core::expressions::SingleNodeExpr;

    use super::*;

    #[test]
    fn nodes() {
        let (instructions, consts) = compile("vstack", &mut ViewIds::new()).unwrap();
        let vm = VirtualMachine::new(instructions, consts);
        let vstack = vm.exec(&mut ViewTemplates::new()).unwrap().remove(0);

        assert!(matches!(vstack, Expression::Node(SingleNodeExpr { .. })));
    }

    #[test]
    fn for_loop() {
        let src = "
        for x in y 
            border
        ";
        let (instructions, consts) = compile(src, &mut ViewIds::new()).unwrap();
        let vm = VirtualMachine::new(instructions, consts);
        let for_loop = vm.exec(&mut ViewTemplates::new()).unwrap().remove(0);

        assert!(matches!(for_loop, Expression::Loop { .. }));
    }
}
