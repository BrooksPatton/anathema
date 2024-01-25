use anathema_widget_core::nodes::Node;
use anathema_widget_core::testing::{test_widget as core_test_widget, FakeTerm};

pub fn test_widget(node: Node, expected: FakeTerm) {
    let _ = crate::register_default_widgets();
    core_test_widget(node, expected);
}
