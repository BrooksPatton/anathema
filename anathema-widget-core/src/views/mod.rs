use std::cell::RefCell;
use std::sync::OnceLock;

use anathema_values::hashmap::HashMap;
use anathema_values::{NodeId, State, ViewId};
use kempt::Map;
use parking_lot::Mutex;

use crate::elements::ViewKind;
use crate::error::{Error, Result};
use crate::{Elements, Event};

pub type ViewFn = dyn FnMut() -> Box<dyn AnyView> + Send;

enum ViewFactory {
    View(Option<Box<dyn AnyView>>),
    Prototype(Box<ViewFn>),
}

static REGISTERED_VIEWS: OnceLock<Mutex<HashMap<ViewId, ViewFactory>>> = OnceLock::new();

type TabIndex = Option<u32>;
thread_local! {
    static TAB_INDICES: RefCell<Map<NodeId, TabIndex>> = RefCell::new(Map::new());
    static VIEWS: RefCell<Map<ViewId, NodeId>> = RefCell::new(Map::new());
}

pub struct RegisteredViews;

impl RegisteredViews {
    pub fn add_view(key: ViewId, view: impl AnyView + 'static) {
        Self::add(key, ViewFactory::View(Some(Box::new(view))));
    }

    pub fn add_prototype<T, F>(key: ViewId, mut f: F)
    where
        F: Send + 'static + FnMut() -> T,
        T: 'static + View + Send,
    {
        Self::add(key, ViewFactory::Prototype(Box::new(move || Box::new(f()))));
    }

    fn add(key: ViewId, view: ViewFactory) {
        REGISTERED_VIEWS
            .get_or_init(Default::default)
            .lock()
            .insert(key, view);
    }

    pub(crate) fn get(id: ViewId) -> Result<(Box<dyn AnyView>, ViewKind)> {
        let mut views = REGISTERED_VIEWS.get_or_init(Default::default).lock();
        let view = views.get_mut(&id);

        match view {
            None => Err(Error::ViewNotFound),
            Some(ViewFactory::Prototype(prototype)) => Ok((prototype(), ViewKind::Prototype)),
            Some(ViewFactory::View(view)) => match view.take() {
                Some(view) => Ok((view, ViewKind::Single(id))),
                None => Err(Error::ViewConsumed),
            },
        }
    }
}

/// NodeIds for views and their tab index
pub struct Views;

impl Views {
    /// Pass a closure that will be called with every node id that belongs
    /// to a view.
    pub fn all<F>(mut f: F) -> Option<NodeId>
    where
        F: FnMut(&mut Map<NodeId, Option<u32>>) -> Option<NodeId>,
    {
        TAB_INDICES.with_borrow_mut(|views| f(views))
    }

    #[doc(hidden)]
    pub fn for_each<F>(mut f: F)
    where
        F: FnMut(&NodeId, Option<u32>),
    {
        TAB_INDICES.with_borrow(|views| {
            views
                .iter()
                .map(|field| (field.key(), field.value))
                .for_each(|(key, value)| f(key, value));
        })
    }

    pub(crate) fn insert(node_id: NodeId, tabindex: Option<u32>) {
        TAB_INDICES.with_borrow_mut(|views| views.insert(node_id, tabindex));
    }

    pub(crate) fn update(node_id: &NodeId, tabindex: Option<u32>) {
        TAB_INDICES.with_borrow_mut(|views| {
            if let Some(old_index) = views.get_mut(node_id) {
                *old_index = tabindex;
            }
        });
    }

    pub(crate) fn associate(view_id: ViewId, node_id: NodeId) {
        VIEWS.with_borrow_mut(|views| views.insert(view_id, node_id));
    }

    pub(crate) fn fetch(view_id: ViewId) -> Option<NodeId> {
        VIEWS.with_borrow(|views| views.get(&view_id).cloned())
    }

    #[cfg(feature = "testing")]
    #[doc(hidden)]
    pub fn test_insert(node_id: impl Into<NodeId>, tab_index: Option<u32>) {
        Self::insert(node_id.into(), tab_index)
    }

    #[cfg(feature = "testing")]
    #[doc(hidden)]
    pub fn test_clear() {
        TAB_INDICES.with_borrow_mut(|views| views.clear());
    }
}

pub trait View {
    type Message;

    /// Called once a view receives an event.
    /// `nodes` represents all the nodes inside the view.
    fn on_event(&mut self, event: Event, _nodes: &mut Elements<'_>) -> Event {
        event
    }

    /// Internal state will always take precedence over external state.
    /// It is not possible to shadow internal state.
    /// This is required to pass internal state to the templates.
    /// Without this no internal state will accessible in the templates.
    fn state(&self) -> &dyn State {
        &()
    }

    /// This function is called every frame
    fn tick(&mut self) {}

    /// This function is called once the view receives focus.
    /// This requires that the view is either the root view, which means it
    /// will receive this call exactly once,
    /// or it is a view with a tab index.
    fn focus(&mut self) {}

    /// This is called when the tab index changes and this view loses focus.
    fn blur(&mut self) {}
}

impl View for () {
    type Message = ();
}

pub trait AnyView: Send {
    fn on_any_event(&mut self, ev: Event, nodes: &mut Elements<'_>) -> Event;

    fn get_any_state(&self) -> &dyn State;

    fn tick_any(&mut self);

    fn focus_any(&mut self);

    fn blur_any(&mut self);
}

impl<T> AnyView for T
where
    T: View + Send,
{
    fn on_any_event(&mut self, event: Event, nodes: &mut Elements<'_>) -> Event {
        self.on_event(event, nodes)
    }

    fn get_any_state(&self) -> &dyn State {
        self.state()
    }

    fn tick_any(&mut self) {
        self.tick();
    }

    fn blur_any(&mut self) {
        self.blur();
    }

    fn focus_any(&mut self) {
        self.focus();
    }
}
