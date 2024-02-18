// pub mod contexts;
// pub mod elements;
pub mod error;
mod event;
// mod factory;
// pub mod layout;
// pub mod nodes;
mod style;
// pub mod views;
// mod widget;

// Need this for the `State` macro
#[allow(unused_extern_crates)]
extern crate anathema_values as anathema;

#[cfg(any(test, feature = "testing"))]
pub mod testing;

// pub use anathema_render::Color;
// pub use elements::{Element, Elements};

// pub use crate::event::{Event, Events, KeyCode, KeyEventState, KeyModifiers, MouseButton};
// pub use crate::factory::{Factory, FactoryContext, WidgetFactory};
// pub use crate::layout::{
//     Align, Axis, Direction, Display, LayoutNode, LayoutNodes, LocalPos, Pos, Region,
// };
// pub use crate::style::WidgetStyle;
// pub use crate::views::View;
// pub use crate::widget::{AnyWidget, Widget, WidgetContainer};
