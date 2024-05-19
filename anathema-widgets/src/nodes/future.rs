use anathema_state::States;
use anathema_store::tree::{NodePath, PathFinder};
use anathema_templates::Globals;

use super::element::Element;
use super::eval::EvalContext;
use super::loops::LOOP_INDEX;
use super::update::scope_value;
use crate::components::ComponentRegistry;
use crate::expressions::{eval, eval_collection};
use crate::values::{Collection, ValueId};
use crate::widget::FloatingWidgets;
use crate::{AttributeStorage, Factory, Scope, WidgetKind, WidgetTree};

struct ResolveFutureValues<'a, 'b, 'bp> {
    globals: &'bp Globals,
    value_id: ValueId,
    factory: &'a Factory,
    scope: &'b mut Scope<'bp>,
    states: &'b mut States,
    components: &'b mut ComponentRegistry,
    attribute_storage: &'b mut AttributeStorage<'bp>,
    floating_widgets: &'b mut FloatingWidgets,
}

impl<'a, 'b, 'bp> PathFinder<WidgetKind<'bp>> for ResolveFutureValues<'a, 'b, 'bp> {
    fn apply(&mut self, node: &mut WidgetKind<'bp>, path: &NodePath, tree: &mut WidgetTree<'bp>) {
        scope_value(node, self.scope, &[]);
        let mut ctx = EvalContext {
            globals: self.globals,
            factory: self.factory,
            scope: self.scope,
            states: self.states,
            components: self.components,
            attribute_storage: self.attribute_storage,
            floating_widgets: self.floating_widgets,
        };
        try_resolve_value(node, &mut ctx, self.value_id, path, tree);
    }

    fn parent(&mut self, parent: &WidgetKind<'bp>, children: &[u16]) {
        scope_value(parent, self.scope, children);
    }
}

pub fn try_resolve_future_values<'bp>(
    globals: &'bp Globals,
    factory: &Factory,
    scope: &mut Scope<'bp>,
    states: &mut States,
    components: &mut ComponentRegistry,
    value_id: ValueId,
    path: &NodePath,
    tree: &mut WidgetTree<'bp>,
    attribute_storage: &mut AttributeStorage<'bp>,
    floating_widgets: &mut FloatingWidgets,
) {
    let res = ResolveFutureValues {
        globals,
        value_id,
        factory,
        scope,
        states,
        components,
        attribute_storage,
        floating_widgets,
    };

    tree.apply_path_finder(path, res);
}

fn try_resolve_value<'bp>(
    widget: &mut WidgetKind<'bp>,
    ctx: &mut EvalContext<'_, '_, 'bp>,
    value_id: ValueId,
    path: &NodePath,
    tree: &mut WidgetTree<'bp>,
) {
    match widget {
        WidgetKind::Element(Element { container, .. }) => {
            let Some(val) = ctx
                .attribute_storage
                .get_mut(container.id)
                .get_mut_with_index(value_id.index())
            else {
                return;
            };
            if let Some(expr) = val.expr {
                let value = eval(expr, ctx.globals, ctx.scope, ctx.states, value_id);
                *val = value;
            }
        }
        WidgetKind::For(for_loop) => {
            // 1. Assign a new collection
            // 2. Remove the current children
            // 3. Build up new children

            for_loop.collection = eval_collection(
                for_loop.collection.expr.unwrap(),
                ctx.globals,
                ctx.scope,
                ctx.states,
                value_id,
            );
            tree.remove_children(path);
            let collection = &for_loop.collection;
            let binding = &for_loop.binding;
            let body = for_loop.body;
            let parent = path;

            for index in 0..collection.len() {
                ctx.scope.push();

                match collection.inner() {
                    Collection::Static(expressions) => {
                        let downgrade = expressions[index].downgrade();
                        ctx.scope.scope_downgrade(binding, downgrade)
                    }
                    Collection::Dyn(value_ref) => {
                        let value = value_ref
                            .as_state()
                            .and_then(|state| state.state_lookup(index.into()))
                            .unwrap(); // TODO: ewwww
                        ctx.scope.scope_pending(binding, value)
                    }
                    Collection::Future => {}
                }

                let iter_id = tree
                    .insert(parent)
                    .commit_child(WidgetKind::Iteration(super::loops::Iteration {
                        loop_index: anathema_state::Value::new(index as i64),
                        binding,
                    }))
                    .unwrap();

                // Scope the iteration value
                tree.with_value(iter_id, |parent, widget, tree| {
                    let WidgetKind::Iteration(iter) = widget else { unreachable!() };
                    ctx.scope.scope_pending(LOOP_INDEX, iter.loop_index.to_pending());

                    for bp in body {
                        crate::eval_blueprint(bp, ctx, parent, tree);
                    }
                });

                ctx.scope.pop();
            }
        }
        WidgetKind::If(widget) => {
            if let Some(expr) = widget.cond.expr {
                let value = eval(expr, ctx.globals, ctx.scope, ctx.states, value_id);
                widget.cond = value;
            }
        }
        WidgetKind::Else(el) => {
            let Some(val) = &mut el.cond else { return };
            if let Some(expr) = val.expr {
                *val = eval(expr, ctx.globals, ctx.scope, ctx.states, value_id);
            }
        }
        WidgetKind::ControlFlow(_) => unreachable!(),
        WidgetKind::Iteration(_) => unreachable!(),
        WidgetKind::Component(component) => {
            let Some(state) = &mut component.external_state else { return };
            for ((_, i), v) in state.iter_mut() {
                if *i == value_id.index() {
                    if let Some(expr) = v.expr {
                        *v = eval(expr, ctx.globals, ctx.scope, ctx.states, value_id);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use anathema_state::{drain_futures, List, Map};
    use anathema_store::stack::Stack;
    use anathema_templates::expressions::{ident, index, list, map, num, strlit};
    use anathema_templates::Expression;

    use super::*;
    use crate::expressions::EvalValue;
    use crate::scope::ScopeLookup;
    use crate::Value;

    fn future_value(expr: &Expression, value_id: ValueId) {
        let globals = Globals::default();
        let scope = Scope::new();
        let states = States::new();
        let mut futures = Stack::empty();

        drain_futures(&mut futures);
        assert_eq!(futures.len(), 0);

        eval(&expr, &globals, &scope, &states, value_id);

        drain_futures(&mut futures);
        assert_eq!(futures.len(), 1);
        let x = futures.pop().unwrap();
        assert_eq!(x, value_id);
    }

    #[test]
    fn ident_future() {
        let expr = ident("val");
        let value_id = ValueId::ONE;
        future_value(&expr, value_id);
    }

    #[test]
    fn list_future() {
        let expr = index(list([num(1), num(2)]), num(1));
        let value_id = ValueId::ONE;
        future_value(&expr, value_id);
    }

    #[test]
    fn map_future() {
        let expr = index(map([("key", strlit("val"))]), ident("key"));
        let value_id = ValueId::ONE;
        future_value(&expr, value_id);
    }

    #[test]
    fn resolve_ident_future() {
        // Template:
        // text some_map.collection[some_value]

        let expr = index(index(ident("some_map"), strlit("collection")), ident("some_value"));

        let value_id = ValueId::ONE;

        let globals = Globals::default();
        let mut scope = Scope::new();
        let mut states = States::new();
        let mut futures = Stack::empty();
        // let ret = eval(&expr, &globals, &scope, &states, value_id);

        // drain_futures(&mut futures);
        // assert_eq!(futures.len(), 1);
        // futures.clear();

        // let ret = eval(&expr, &globals, &scope, &states, value_id);
        // assert_eq!(*ret, EvalValue::Empty);
        // futures.clear();
        // assert_eq!(futures.len(), 0);

        let mut some_map = Map::<List<_>>::empty();
        let mut list = List::empty();
        list.push_back(123u32);
        some_map.insert("collection", list);

        let mut state = Map::<Map<_>>::empty();
        state.insert("some_map", some_map);
        let sid1 = states.insert(Box::new(state));
        scope.insert_state(sid1);

        let mut state = Map::<usize>::empty();
        state.insert("some_value", 0);

        let sid2 = states.insert(Box::new(state));
        scope.insert_state(sid2);

        let lookup = ScopeLookup::new("some_map", Some(value_id));

        let foolery = scope.get(lookup, &mut None, &states);

        let ret = eval(&expr, &globals, &scope, &states, value_id);
        assert_eq!(*ret, EvalValue::Empty);

        drain_futures(&mut futures);
        assert_eq!(futures.len(), 1);
    }
}
