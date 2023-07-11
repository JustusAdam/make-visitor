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
use syn::visit::Visit;
use syn::{Variant, Visibility};
use syn::{Token,
    parse_macro_input, punctuated::Punctuated, Field, Fields, GenericArgument, Index, Item, Member,
    Path, PathArguments, Type, TypeArray, TypeSlice,
};

struct Options<'ast> {
    generate_variant_visitors: bool,
    in_mod: Option<Ident>,
    visibility: &'ast Visibility,
}


struct Generator<'a> {
    options: Options<'a>,
    available_types: HashSet<&'a Ident>,
    visitor_methods: Vec<TokenStream>,
    visit_functions: Vec<TokenStream>,
    variant_stack: Option<Vec<(&'a Variant, TokenStream)>>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Generator<'a> {
    fn new(options: Options<'a>, available_types: HashSet<&'a Ident>) -> Self {
        Self {
            options,
            available_types,
            visit_functions: vec![],
            visitor_methods: vec![],
            variant_stack: None,
            diagnostics: vec![],
        }
    }
}

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

impl<'ast> Visit<'ast> for Generator<'ast> {
    fn visit_item_struct(&mut self, strct: &'ast syn::ItemStruct) {
        let visit_name = to_visit_method(&strct.ident);
        let ty = &strct.ident;
        let visit_fields = strct
            .fields
            .iter()
            .filter_map(|f| {
                let field = &f.ident;
                self.handle_type(&f.ty, &quote!(&i.#field)).ok()
            })
            .collect::<Vec<_>>();
        let vis = self.options.visibility;
        self.add_visitor_method(quote!(
            fn #visit_name(&mut self, i: &#ty) {
                #visit_name(self, i)
            }
        ));
        self.add_visit_function(quote!(
            #vis fn #visit_name<V: Visitor + ?Sized>(v: &mut V, i: &#ty) {
                #(#visit_fields);*
            }
        ))
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        let empty = Punctuated::new();
        let fields = match &variant.fields {
            Fields::Named(flds) => &flds.named,
            Fields::Unnamed(flds) => &flds.unnamed,
            Fields::Unit => &empty,
        };
        let name = to_visit_method(&variant.ident);
        let args = field_names(fields.iter()).collect::<Vec<_>>();
        let ref types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();
        let method_calls = args
            .iter()
            .zip(types)
            .filter_map(|(field, ty)| self.handle_type(ty, &quote!(#field)).ok())
            .collect::<Vec<_>>();
        let vis = self.options.visibility;
        let inner = if self.options.generate_variant_visitors {
            self.add_visitor_method(quote!(
                fn #name(&mut self, #(#args:&#types),*) {
                    #name(self, #(#args),*)
                }
            ));
            self.add_visit_function(quote!(
                #vis fn #name<V:Visitor + ?Sized>(v: &mut V, #(#args : &#types),*) {
                    #(
                        #method_calls
                    );*
                }
            ));
            quote!(v.#name(#(#args),*))
        } else {
            quote!({
                #(#method_calls);*
            })
        };
        self.variant_stack.as_mut().unwrap().push((variant, inner));
    }

    fn visit_item_enum(&mut self, enm: &'ast syn::ItemEnum) {
        assert!(self.variant_stack.replace(vec![]).is_none());
        syn::visit::visit_item_enum(self, enm);
        let stack = self.variant_stack.take().unwrap();
        let visit_name = to_visit_method(&enm.ident);
        let enum_name = &enm.ident;
        let matches = stack.into_iter().map(|(variant, inner)| {
            let empty = Punctuated::new();
            let fields = match &variant.fields {
                Fields::Named(flds) => &flds.named,
                Fields::Unnamed(flds) => &flds.unnamed,
                Fields::Unit => &empty,
            };
            let var_name = &variant.ident;
            let fields = field_names(fields.iter()).collect::<Vec<_>>();
            match &variant.fields {
                Fields::Named(_) => {
                    quote!(#enum_name::#var_name{ #(#fields),* } => #inner)
                }
                Fields::Unnamed(_) => {
                    quote!(#enum_name::#var_name(#(#fields),*) => #inner)
                }
                Fields::Unit => quote!(#enum_name::#var_name => ()),
            }
        });
        let vis = self.options.visibility;
        self.add_visitor_method(quote!(
            fn #visit_name(&mut self, i: &#enum_name) {
                #visit_name(self, i)
            }
        ));
        self.add_visit_function(quote!(
            #vis fn #visit_name<V: Visitor + ?Sized>(v: &mut V, i: &#enum_name) {
                match i {
                    #(#matches),*
                }
            }
        ));
    }
}

impl<'a> Generator<'a> {
    fn add_visit_function(&mut self, function: TokenStream) {
        self.visit_functions.push(function)
    }

    fn add_visitor_method(&mut self, function: TokenStream) {
        self.visitor_methods.push(function)
    }

    fn handle_array_like(
        &self,
        elem: &Type,
        target: &TokenStream,
    ) -> Result<TokenStream, Diagnostic> {
        let inner = self.handle_type(&elem, &quote!(it))?;
        Ok(quote!(
            for it in (#target).iter() {
                #inner
            }
        ))
    }

    fn handle_type(&self, typ: &Type, target: &TokenStream) -> Result<TokenStream, Diagnostic> {
        let target = &target;
        match typ {
            Type::Array(TypeArray { elem, .. }) | Type::Slice(TypeSlice { elem, .. }) => {
                self.handle_array_like(elem.as_ref(), target)
            }
            Type::Paren(ty) => self.handle_type(&ty.elem, target),
            Type::Reference(rf) => {
                let inner = rf.elem.as_ref();
                match inner {
                    Type::Slice(slc) => self.handle_array_like(&slc.elem, target),
                    _ => self.handle_type(inner, &quote!(*#target)),
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
                        self.handle_type(ty, &quote!(&(#target).#idx))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(quote!(#(#for_fields);*))
            }
            Type::Path(path) => path
                .path
                .get_ident()
                .and_then(|id| {
                    self.available_types.contains(id).then(|| {
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
                    .and_then(|ty| self.handle_array_like(ty, target).ok())
                })
                .or_else(|| {
                    matches_tail_generic(&[&["Box"], &["std", "box", "Box"]], &path.path)
                        .and_then(single_generic_ty)
                        .and_then(|ty| self.handle_type(ty, &quote!((#target).as_ref())).ok())
                })
                .ok_or_else(|| typ.span().unwrap().warning("Unknown path type")),
            _ => Err(typ.span().unwrap().warning("Unsupported type")),
        }
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

    let requested_visibility = items.iter().filter_map(|it| 
        match it {
            Item::Enum(e) => Some(&e.vis),
            Item::Struct(s) => Some(&s.vis),
            _ => None,
        }
    ).reduce(|vis1, vis2| {
        match (vis1, vis2) {
            (Visibility::Inherited, _) => vis1,
            (Visibility::Public(_), _) => vis2,
            (_, Visibility::Public(_)) => vis1,
            (_, Visibility::Inherited) => vis2,
            (Visibility::Restricted(r1), Visibility::Restricted(r2)) => {
                if r2.path.leading_colon.is_some() == r2.path.leading_colon.is_some()
                    && r1.path.segments.iter().zip(r2.path.segments.iter()).all(|(p1, p2)| p1 == p2) {
                    if r1.path.segments.len() > r2.path.segments.len() {
                        vis1
                    } else {
                        vis2
                    }
                } else {
                    vis1.span().join(vis2.span()).unwrap().unwrap().error("Unreconsilable visibilities").emit();
                    panic!()
                }
            }
        }
    });

    let default_vis = Visibility::Public(Token![pub](Span::call_site()));

    let mut options = 
        Options {
            generate_variant_visitors: false,
            in_mod: Some(syn::parse_str("visit").unwrap()),
            visibility: &default_vis,
        };

    // requested_visibility.map(|vis|
    //     options.visibility = vis
    // );

    let mut generator = Generator::new(options, available_types);

    for i in &items {
        generator.visit_item(i);
    }

    let visit_methods = generator.visitor_methods;
    let visit_functions = generator.visit_functions;

    let has_diags = !generator.diagnostics.is_empty();
    for diag in generator.diagnostics {
        diag.emit();
    }
    if has_diags {
        panic!()
    }

    let all_functions = quote!(
        pub trait Visitor {
            #(#visit_methods)*
        }

        #(#visit_functions)*
    );

    let available_types = generator.available_types.iter();

    let inner = if let Some(path) = generator.options.in_mod {
        quote!(
            mod #path {
                use super::{
                    #(#available_types),*
                };
                #all_functions
            }
        )
    } else {
        all_functions
    };

    quote!(
        #(#items)*

        #inner
    )
    .into()
}
