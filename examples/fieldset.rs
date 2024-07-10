use anathema::prelude::*;
use std::fs::read_to_string;

fn main() {
    let template = read_to_string("examples/fieldset.aml").expect("loading template");

    let mut doc = Document::new(template);

    let mut backend = TuiBackend::builder()
        .enable_alt_screen()
        .enable_raw_mode()
        .hide_cursor()
        .finish()
        .unwrap();

    let mut runtime = Runtime::new(doc, backend).finish().expect("setting up runtime");

    runtime.run();
}
