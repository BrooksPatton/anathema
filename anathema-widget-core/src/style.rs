use anathema_render::{Attributes, Color, Style as RenderStyle};
use anathema_values::{Context, NodeId, Value};

#[derive(Debug)]
pub struct WidgetStyle {
    pub(crate) fg: Value<Color>,
    pub(crate) bg: Value<Color>,
    pub(crate) bold: Value<bool>,
    pub(crate) dim: Value<bool>,
    pub(crate) italic: Value<bool>,
    pub(crate) underlined: Value<bool>,
    pub(crate) crossed_out: Value<bool>,
    pub(crate) overlined: Value<bool>,
    pub(crate) inverse: Value<bool>,
}

impl WidgetStyle {
    pub fn style(&self, node_id: &NodeId) -> RenderStyle {
        panic!("figure out what to do with node id here");
        // let mut attributes: Attributes = Attributes::empty();

        // if let Some(true) = self.bold.value_ref(node_id) {
        //     attributes |= Attributes::BOLD;
        // }

        // if let Some(true) = self.dim.value_ref(node_id) {
        //     attributes |= Attributes::DIM;
        // }

        // if let Some(true) = self.italic.value_ref(node_id) {
        //     attributes |= Attributes::ITALIC;
        // }

        // if let Some(true) = self.underlined.value_ref(node_id) {
        //     attributes |= Attributes::UNDERLINED;
        // }

        // if let Some(true) = self.crossed_out.value_ref(node_id) {
        //     attributes |= Attributes::CROSSED_OUT;
        // }

        // if let Some(true) = self.overlined.value_ref(node_id) {
        //     attributes |= Attributes::OVERLINED;
        // }

        // if let Some(true) = self.inverse.value_ref(node_id) {
        //     attributes |= Attributes::INVERSE;
        // }

        // RenderStyle {
        //     fg: self.fg.value_ref(node_id),
        //     bg: self.bg.value_ref(node_id),
        //     attributes,
        // }
    }

    pub fn resolve(&mut self, context: &Context<'_>, node_id: &NodeId) {
        panic!();
        // self.fg.resolve(context, node_id);
        // self.bg.resolve(context, node_id);
        // self.bold.resolve(context, node_id);
        // self.dim.resolve(context, node_id);
        // self.italic.resolve(context, node_id);
        // self.underlined.resolve(context, node_id);
        // self.crossed_out.resolve(context, node_id);
        // self.overlined.resolve(context, node_id);
        // self.inverse.resolve(context, node_id);
    }
}
