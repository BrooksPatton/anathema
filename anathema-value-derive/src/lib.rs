use manyhow::{ensure, manyhow, Result};
use quote_use::quote_use as quote;
use syn::{self, Fields};

#[manyhow]
#[proc_macro_derive(State)]
pub fn state_derive(strct: syn::ItemStruct) -> Result {
    let name = &strct.ident;

    ensure!(
        let Fields::Named(struct_fields) = &strct.fields,
        strct.fields,
        "only named fields"
    );

    let (field_idents, field_names): (Vec<_>, Vec<_>) = struct_fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .map(|f| (f, f.to_string()))
        .unzip();

    Ok(quote! {
        # use ::anathema::values::{self, Value, ValueRef, Path, state};
        impl state::State for #name {
            fn state_get(&self, key: values::Path<'_>, node_id: &values::NodeId) -> Option<ValueRef> {
                match key {
                    Path::Key(s) => match s {
                        #(
                            #field_names => {
                                Some(self.#field_idents.value_ref(node_id.clone()))
                            }
                        )*
                        _ => None,
                    }
                    _ => None,
                }
            }
        }

        // impl<'a> Into<ValueRef> for &#name {
        //     fn into(self) -> ::anathema::values::ValueRef {
        //         ::anathema::values::ValueRef::Map(self)
        //     }
        // }
    })
}
