#![feature(proc_macro_diagnostic)]

extern crate either;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::{Diagnostic, TokenStream as TokenStream2};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::collections::HashSet;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, punctuated::Punctuated, Field, Fields, GenericArgument, Index, Item, Member,
    Path, PathArguments, Type, TypeArray, TypePath, TypeSlice,
};

fn as_camel(s: &str) -> String {
    let mut new = String::with_capacity(s.len());
    let mut is_begin = true;
    for c in s.chars() {
        if c.is_uppercase() {
            if !is_begin {
                new.push('_');
            }
            new.extend(c.to_lowercase());
        } else {
            new.push(c);
        }
        is_begin = false;
    }
    new
}

fn to_visit_method(ident: &Ident) -> Ident {
    Ident::new(
        &format!("visit_{}", as_camel(ident.to_string().as_str())),
        Span::call_site(),
    )
}

fn field_names<'a>(
    fields: impl IntoIterator<Item = &'a Field> + 'a,
) -> impl Iterator<Item = Ident> + 'a {
    fields.into_iter().enumerate().map(|(index, field)| {
        Ident::new(
            &field
                .ident
                .as_ref()
                .map_or_else(|| format!("field_{index}"), |f| f.to_string()),
            Span::call_site(),
        )
    })
}

fn handle_variant(
    available_types: &HashSet<&Ident>,
    ident: &Ident,
    fields: &Fields,
) -> Result<(TokenStream, TokenStream), Diagnostic> {
    let name = to_visit_method(&ident);
    let args = field_names(fields.iter()).collect::<Vec<_>>();
    let ref types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

    let method_calls = args
        .iter()
        .zip(types)
        .filter_map(|(field, ty)| handle_type(available_types, ty, &quote!(#field)).ok());
    Ok((
        quote!(
            fn #name(&mut self, #(#args:&#types),*) {
                #name(self, #(#args),*)
            }
        ),
        quote!(
            pub fn #name<V:Visitor + ?Sized>(v: &mut V, #(#args : &#types),*) {
                #(
                    #method_calls
                );*
            }
        ),
    ))
}

fn handle_array_like(
    available_types: &HashSet<&Ident>,
    elem: &Type,
    target: &TokenStream,
) -> Result<TokenStream, Diagnostic> {
    let inner = handle_type(available_types, &elem, &quote!(it))?;
    Ok(quote!(
        for it in (#target).iter() {
            #inner
        }
    ))
}

fn matches_tail_generic<'a>(
    options: &[&[&str]],
    path: &'a Path,
) -> Option<impl Iterator<Item = &'a GenericArgument>> {
    options.iter().find_map(|opt| {
        (opt.len() == path.segments.len()).then_some(())?;
        let mut it = path.segments.iter().rev();
        let last = it.next()?;
        it.all(|segment| segment.arguments.is_empty())
            .then_some(())?;
        path.segments
            .iter()
            .zip(opt.iter())
            .all(|(one, other)| one.ident == other)
            .then_some(())?;
        match &last.arguments {
            PathArguments::AngleBracketed(args) => Some(args.args.iter()),
            _ => None,
        }
    })
}

fn single_generic_ty<'a>(
    mut generics: impl Iterator<Item = &'a GenericArgument>,
) -> Option<&'a Type> {
    let GenericArgument::Type(inner) = generics.next()? else {
                        return None;
                    };
    generics.next().is_none().then_some(&inner)
}

fn handle_type(
    available_types: &HashSet<&Ident>,
    typ: &Type,
    target: &TokenStream,
) -> Result<TokenStream, Diagnostic> {
    let target = &target;
    match typ {
        Type::Array(TypeArray { elem, .. }) | Type::Slice(TypeSlice { elem, .. }) => {
            handle_array_like(available_types, elem.as_ref(), target)
        }
        Type::Paren(ty) => handle_type(available_types, &ty.elem, target),
        Type::Reference(rf) => {
            let inner = rf.elem.as_ref();
            match inner {
                Type::Slice(slc) => handle_array_like(available_types, &slc.elem, target),
                _ => handle_type(available_types, inner, &quote!(*#target)),
            }
        }
        Type::Tuple(tup) => {
            let for_fields = tup
                .elems
                .iter()
                .enumerate()
                .map(|(index, ty)| {
                    let idx = Member::Unnamed(Index {
                        index: index as u32,
                        span: Span::call_site(),
                    });
                    handle_type(available_types, ty, &quote!(&(#target).#idx))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(quote!(#(#for_fields);*))
        }
        Type::Path(path) => path
            .path
            .get_ident()
            .and_then(|id| {
                available_types.contains(id).then(|| {
                    let visit_name = to_visit_method(id);
                    quote!(v.#visit_name(#target))
                })
            })
            .or_else(|| {
                matches_tail_generic(
                    &[&["Vec"], &["std", "alloc", "vec"], &["std", "vec", "vec"]],
                    &path.path,
                )
                .and_then(single_generic_ty)
                .and_then(|ty| handle_array_like(available_types, ty, target).ok())
            })
            .or_else(|| {
                matches_tail_generic(&[&["Box"], &["std", "box", "Box"]], &path.path)
                    .and_then(single_generic_ty)
                    .and_then(|ty| {
                        handle_type(available_types, ty, &quote!((#target).as_ref())).ok()
                    })
            })
            .ok_or_else(|| typ.span().unwrap().warning("Unknown path type")),
        _ => Err(typ.span().unwrap().warning("Unsupported type")),
    }
}

fn handle_item(
    available_types: &HashSet<&Ident>,
    item: &syn::Item,
) -> Result<(TokenStream, TokenStream), Diagnostic> {
    match item {
        Item::Struct(strct) => {
            let visit_name = to_visit_method(&strct.ident);
            let ty = &strct.ident;
            let visit_fields = strct.fields.iter().filter_map(|f| {
                let field = &f.ident;
                handle_type(available_types, &f.ty, &quote!(&i.#field)).ok()
            });
            Ok((
                quote!(
                    fn #visit_name(&mut self, i: &#ty) {
                        #visit_name(self, i)
                    }
                ),
                quote!(
                    fn #visit_name<V: Visitor + ?Sized>(v: &mut V, i: &#ty) {
                        #(#visit_fields);*
                    }
                ),
            ))
        }
        Item::Enum(enm) => {
            let visit_name = to_visit_method(&enm.ident);
            let enum_name = &enm.ident;
            let matches = enm.variants.iter().map(|var| {
                let empty = Punctuated::new();
                let fields = match &var.fields {
                    Fields::Named(flds) => &flds.named,
                    Fields::Unnamed(flds) => &flds.unnamed,
                    Fields::Unit => &empty,
                };
                let var_name = &var.ident;
                let handle_var = to_visit_method(var_name);
                let fields = field_names(fields.iter()).collect::<Vec<_>>();
                match &var.fields {
                    Fields::Named(_) => {
                        quote!(#enum_name::#var_name{ #(#fields),* } => v.#handle_var(#(#fields),*))
                    }
                    Fields::Unnamed(_) => {
                        quote!(#enum_name::#var_name(#(#fields),*) => v.#handle_var(#(#fields),*))
                    }
                    Fields::Unit => quote!(#enum_name::#var_name => ()),
                }
            });
            let selfvisit = (
                quote!(
                    fn #visit_name(&mut self, i: &#enum_name) {
                        #visit_name(self, i)
                    }
                ),
                quote!(
                    fn #visit_name<V: Visitor + ?Sized>(v: &mut V, i: &#enum_name) {
                        match i {
                            #(#matches),*
                        }
                    }
                ),
            );
            enm.variants
                .iter()
                .map(|var| handle_variant(available_types, &var.ident, &var.fields))
                .chain([Ok(selfvisit)])
                .collect::<Result<Vec<_>, _>>()
                .map(|res| res.into_iter().unzip())
        }
        _ => Err(item
            .span()
            .unwrap()
            .error("Unsupported item type, expected struct or enum")),
    }
}

struct Definitions(Vec<Item>);

impl syn::parse::Parse for Definitions {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Definitions(
            std::iter::from_fn(|| (!input.is_empty()).then(|| input.parse()))
                .collect::<Result<_, _>>()?,
        ))
    }
}

#[proc_macro]
pub fn make_visitor(items: TokenStream2) -> TokenStream2 {
    let items = parse_macro_input!(items as Definitions).0;

    let available_types = items
        .iter()
        .filter_map(|item| match item {
            Item::Enum(e) => Some(&e.ident),
            Item::Struct(s) => Some(&s.ident),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let (visit_methods, visit_functions): (TokenStream, TokenStream) = match items
        .iter()
        .map(|item| handle_item(&available_types, item))
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(visitors) => visitors.into_iter().unzip(),
        Err(diagnostic) => {
            diagnostic.emit();
            panic!()
        }
    };

    let out = quote!(
        #(#items)*

        trait Visitor {
            #visit_methods
        }

        #visit_functions
    );
    print!("{out}");
    out.into()
}
