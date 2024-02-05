// -----------------------------------------------------------------------------
//   - Basic view -
//   Load and display a template
// -----------------------------------------------------------------------------
use std::fs::read_to_string;

use anathema::runtime::Runtime;
use anathema::vm::Templates;

fn main() {
    // Step one: Load and compile templates
    let template = read_to_string("examples/templates/basic.aml").unwrap();
    let mut templates = Templates::new(template, ());
    let templates = templates.compile().unwrap();

    // Step two: Runtime
    let mut runtime = Runtime::new(&templates).unwrap();
    runtime.enable_alt_screen = false;

    // Step three: start the runtime
    runtime.run().unwrap();
}
