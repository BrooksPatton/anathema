use std::ops::{ControlFlow, Deref, DerefMut};

use anathema_render::Size;
use anathema_values::Context;

use super::Constraints;
use crate::error::Result;
use crate::{Elements, WidgetContainer};

#[derive(Debug)]
pub struct LayoutNodes<'nodes, 'state, 'expr> {
    pub nodes: &'nodes mut Elements<'expr>,
    pub constraints: Constraints,
    pub context: &'nodes mut Context<'state, 'expr>,
}

impl<'nodes, 'state, 'expr> LayoutNodes<'nodes, 'state, 'expr> {
    pub fn new(
        nodes: &'nodes mut Elements<'expr>,
        constraints: Constraints,
        context: &'nodes mut Context<'state, 'expr>,
    ) -> Self {
        Self {
            nodes,
            constraints,
            context,
        }
    }

    pub fn set_constraints(&mut self, constraints: Constraints) {
        self.constraints = constraints;
    }

    pub fn next<F>(&mut self, mut f: F) -> Result<()>
    where
        F: FnMut(LayoutNode<'_, 'expr>, &mut Context<'_, 'expr>) -> Result<()>,
    {
        self.nodes
            .next(self.context, &mut |widget, children, context| {
                let node = LayoutNode {
                    widget,
                    children,
                };
                f(node, context)
            })?;

        Ok(())
    }

    pub fn for_each<F>(&mut self, mut f: F) -> Result<()>
    where
        F: FnMut(LayoutNode<'_, 'expr>, &mut Context<'_, 'expr>) -> Result<()>,
    {
        loop {
            let res = self
                .nodes
                .next(self.context, &mut |widget, children, context| {
                    let node = LayoutNode {
                        widget,
                        children,
                    };
                    f(node, context)
                })?;

            match res {
                ControlFlow::Break(()) => break Ok(()),
                ControlFlow::Continue(()) => continue,
            }
        }
    }

    pub fn filter<F>(&mut self, f: F) -> impl Iterator<Item = LayoutNode<'_, 'expr>> + '_
    where
        F: Fn(&WidgetContainer<'expr>) -> bool + 'static,
    {
        self.nodes
            .iter_mut()
            .filter(move |(widget, _)| f(widget))
            .map(|(widget, children)| LayoutNode {
                widget,
                children,
            })
    }
}

pub struct LayoutNode<'widget, 'expr> {
    widget: &'widget mut WidgetContainer<'expr>,
    children: &'widget mut Elements<'expr>,
}

impl<'widget, 'expr> LayoutNode<'widget, 'expr> {
    pub fn layout(
        &mut self,
        constraints: Constraints,
        context: &mut Context<'_, 'expr>,
    ) -> Result<Size> {
        self.widget.layout(self.children, constraints, context)
    }
}

impl<'widget, 'expr> From<(&'widget mut WidgetContainer<'expr>, &'widget mut Elements<'expr>)> for LayoutNode<'widget, 'expr> {
    fn from((widget, children): (&'widget mut WidgetContainer<'expr>, &'widget mut Elements<'expr>)) -> Self {
        Self {
            widget,
            children,
        }
    }
}

impl<'widget, 'expr> Deref for LayoutNode<'widget, 'expr> {
    type Target = WidgetContainer<'expr>;

    fn deref(&self) -> &Self::Target {
        self.widget
    }
}

impl<'widget, 'expr> DerefMut for LayoutNode<'widget, 'expr> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.widget
    }
}
