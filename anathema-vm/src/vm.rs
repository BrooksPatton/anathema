use anathema_compiler::Instruction;
use anathema_values::{Constants, Variables};
use anathema_widget_core::nodes::Node;

use crate::error::Result;
use crate::scope::Scope;
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

    pub(super) fn exec(self, views: &mut ViewTemplates, globals: &mut Variables) -> Result<Vec<Node>> {
        let mut root_scope = Scope::new(self.instructions, &self.consts);
        let mut vars = Variables::new();
        root_scope.exec(views, &mut vars, globals)
    }
}

#[cfg(test)]
mod test {
    use anathema_compiler::compile;
    use anathema_values::ViewIds;

    use super::*;

    #[test]
    fn nodes() {
        let (instructions, consts) = compile("vstack", &mut ViewIds::new()).unwrap();
        let mut globals = Variables::new();
        let vm = VirtualMachine::new(instructions, consts);
        let vstack = vm.exec(&mut ViewTemplates::new(), &mut globals).unwrap().remove(0);

        assert!(matches!(vstack, Node::Single(..)));
    }

    #[test]
    fn for_loop() {
        let src = "
        for x in y 
            border
        ";
        let (instructions, consts) = compile(src, &mut ViewIds::new()).unwrap();
        let mut globals = Variables::new();
        let vm = VirtualMachine::new(instructions, consts);
        let for_loop = vm.exec(&mut ViewTemplates::new(), &mut globals).unwrap().remove(0);

        assert!(matches!(for_loop, Node::Loop { .. }));
    }
}
