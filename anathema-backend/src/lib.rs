use std::ops::ControlFlow;
use std::time::Duration;

use anathema_geometry::{Pos, Size};
use anathema_value_resolver::{AttributeStorage, Scope};
use anathema_widgets::components::events::Event;
use anathema_widgets::error::Result;
use anathema_widgets::layout::{Constraints, LayoutCtx, LayoutFilter, PositionFilter, Viewport};
use anathema_widgets::paint::PaintFilter;
use anathema_widgets::{
    DirtyWidgets, GlyphMap, Layout, LayoutForEach, PaintChildren, PositionChildren, WidgetTreeView,
};

pub mod testing;
pub mod tui;

pub trait Backend {
    fn size(&self) -> Size;

    fn next_event(&mut self, timeout: Duration) -> Option<Event>;

    fn resize(&mut self, new_size: Size, glyph_map: &mut GlyphMap);

    /// Paint the widgets
    fn paint<'bp>(
        &mut self,
        glyph_map: &mut GlyphMap,
        widgets: PaintChildren<'_, 'bp>,
        attribute_storage: &AttributeStorage<'bp>,
    );

    /// Called by the runtime at the end of the frame.
    fn render(&mut self, glyph_map: &mut GlyphMap);

    /// Clear is called immediately after `render` is called.
    fn clear(&mut self);

    /// Finalizes the backend. This is called when the runtime starts.
    fn finalize(&mut self) {}
}

// TODO: rename this.
// This does layout, position and paint and should have
// a less silly name
pub struct WidgetCycle<'rt, 'bp, T> {
    backend: &'rt mut T,
    tree: WidgetTreeView<'rt, 'bp>,
    constraints: Constraints,
}

impl<'rt, 'bp, T: Backend> WidgetCycle<'rt, 'bp, T> {
    pub fn new(backend: &'rt mut T, tree: WidgetTreeView<'rt, 'bp>, constraints: Constraints) -> Self {
        Self {
            backend,
            tree,
            constraints,
        }
    }

    fn fixed(&mut self, ctx: &mut LayoutCtx<'_, 'bp>) -> Result<()> {
        // -----------------------------------------------------------------------------
        //   - Position -
        // -----------------------------------------------------------------------------
        self.position(ctx.attribute_storage, *ctx.viewport, PositionFilter::fixed());

        // -----------------------------------------------------------------------------
        //   - Paint -
        // -----------------------------------------------------------------------------
        self.paint(ctx, PaintFilter::fixed());

        Ok(())
    }

    fn floating(&mut self, ctx: &mut LayoutCtx<'_, 'bp>) -> Result<()> {
        // -----------------------------------------------------------------------------
        //   - Position -
        // -----------------------------------------------------------------------------
        self.position(ctx.attribute_storage, *ctx.viewport, PositionFilter::floating());

        // -----------------------------------------------------------------------------
        //   - Paint -
        // -----------------------------------------------------------------------------
        self.paint(ctx, PaintFilter::floating());

        Ok(())
    }

    pub fn run(
        &mut self,
        ctx: &mut LayoutCtx<'_, 'bp>,
        force_layout: bool,
        dirty_widgets: &mut DirtyWidgets,
    ) -> Result<()> {
        // -----------------------------------------------------------------------------
        //   - Layout -
        // -----------------------------------------------------------------------------
        self.layout(ctx, LayoutFilter, dirty_widgets, force_layout)?;

        // -----------------------------------------------------------------------------
        //   - Position and paint -
        // -----------------------------------------------------------------------------
        self.fixed(ctx)?;
        self.floating(ctx)?;
        Ok(())
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx<'_, 'bp>,
        filter: LayoutFilter,
        dirty_widgets: &mut DirtyWidgets,
        force_layout: bool,
    ) -> Result<()> {
        #[cfg(feature = "profile")]
        puffin::profile_function!();

        let mut tree = self.tree.view();

        if force_layout {
            // Perform a layout across the entire tree
            let scope = Scope::root();
            let mut for_each = LayoutForEach::new(tree, &scope, filter);
            let constraints = self.constraints;
            _ = for_each.each(ctx, |ctx, widget, children| {
                _ = widget.layout(children, constraints, ctx)?;
                Ok(ControlFlow::Break(()))
            })?;
            return Ok(());
        }

        // If a widget has changed, mark the parent as dirty

        // Layout only changed widgets.
        // These are the parents of changed widgets.
        //
        // Investigate the possibility of attaching an offset as existing widgets don't need
        // to reflow unless the constraint has changed.
        //
        // This means `additional_widgets` needs to store (key, offset) where offset can be None
        //
        // If this is going to work we need to consider `expand` and `spacer`
        //
        // Since widgets can be made by anyone and they are always guaranteed to give
        // access to all their children this might not be a possibility.
        //
        // parent
        //     widget 0
        //     widget 1
        //     widget 2 |  <- if this changes, only reflow this, three and four
        //     widget 3 |-- reflow
        //     widget 4 |

        // TODO: make `additional_widgets` a scratch buffer part of `DirtyWidgets`.
        //       Also ensure that it tracks last id as well
        //       ... and removes it when done!
        let mut additional_widgets = vec![];

        loop {
            for widget_id in dirty_widgets.drain() {
                if !tree.contains(widget_id) {
                    continue;
                }
                tree.with_value_mut(widget_id, |_, widget, children| {
                    let scope = Scope::root();
                    let mut children = LayoutForEach::new(children, &scope, filter);
                    children.parent_element = Some(widget_id);
                    let parent_id = widget.parent_widget;
                    let anathema_widgets::WidgetKind::Element(widget) = &mut widget.kind else { return };

                    let constraints = widget.constraints();
                    if let Ok(Layout::Changed(_)) = widget.layout(children, constraints, ctx) {
                        // write into scratch buffer
                        if let Some(id) = parent_id {
                            additional_widgets.push(id);
                        }
                    }
                });
            }

            // merge the scratch if it's not empty

            if additional_widgets.is_empty() {
                break;
            }

            dirty_widgets.inner.append(&mut additional_widgets);
        }

        Ok(())
    }

    fn position(&mut self, attributes: &AttributeStorage<'bp>, viewport: Viewport, filter: PositionFilter) {
        #[cfg(feature = "profile")]
        puffin::profile_function!();

        let mut for_each = PositionChildren::new(self.tree.view(), attributes, filter);
        _ = for_each.each(|widget, children| {
            widget.position(children, Pos::ZERO, attributes, viewport);
            ControlFlow::Break(())
        });
    }

    fn paint(&mut self, ctx: &mut LayoutCtx<'_, 'bp>, filter: PaintFilter) {
        #[cfg(feature = "profile")]
        puffin::profile_function!();

        let for_each = PaintChildren::new(self.tree.view(), ctx.attribute_storage, filter);
        self.backend.paint(ctx.glyph_map, for_each, ctx.attribute_storage);
    }
}
