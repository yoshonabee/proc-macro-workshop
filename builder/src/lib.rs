use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Type, TypePath};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let builder_name = format_ident!("{name}Builder");
    let vis = &input.vis;

    let mut field_funcs = Vec::new();
    let mut fields_def = Vec::new();
    let mut build_def = Vec::new();

    // let meta_lll = Vec::new();

    if let syn::Data::Struct(data) = &input.data {
        if let syn::Fields::Named(fields) = &data.fields {
            for field in fields.named.iter() {
                let field_name = field.ident.as_ref().expect("field should have Identity");
                let field_ty = field.ty.clone();

                if let Some(ty) = extract_type_from_option(&field_ty) {
                    field_funcs.push(quote! {
                        #vis fn #field_name(&mut self, input: #ty) -> &mut Self {
                            self.#field_name = Some(Some(input));
                            self
                        }
                    });

                    build_def.push(quote! {
                        #field_name: self.#field_name.clone().unwrap_or(None),
                    })
                } else {
                    field_funcs.push(quote! {
                        #vis fn #field_name(&mut self, input: #field_ty) -> &mut Self {
                            self.#field_name = Some(input);
                            self
                        }
                    });

                    build_def.push(quote! {
                        #field_name: self.#field_name.clone().unwrap(),
                    })
                }

                fields_def.push(quote! {
                    #field_name: Option<#field_ty>,
                });

                for attr in field.attrs.iter() {
                    if attr.path().is_ident("builder") {
                        let _ = attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("each") {
                                let value = meta.value()?;
                                let s: syn::LitStr = value.parse()?;
                                let each_name = s.value();
                                let each_name_ident = syn::Ident::new(&each_name, proc_macro2::Span::call_site());
                                let q = quote! {
                                    #vis fn #each_name_ident(&mut self, input: #field_ty) -> &mut Self {
                                        if let None = self.#field_name {
                                            self.#field_name = Some(#field_ty);
                                        }
                                        self.#field_name.push(input);
                                        self
                                    }
                                };
                                println!("{:#?}", q.to_string());
                                field_funcs.push(q);
                                Ok(())
                            } else {
                                Err(meta.error("unsupported attribute"))
                            }
                        });
                    }
                }
            }
        } else {
            panic!("Not implemented for unnamed fields")
        }
    }

    let field_funcs = proc_macro2::TokenStream::from_iter(field_funcs);
    let fields_def = proc_macro2::TokenStream::from_iter(fields_def);
    let build_def = proc_macro2::TokenStream::from_iter(build_def);

    let expanded = quote! {
        use std::error::Error;

        #[derive(Default)]
        #vis struct #builder_name {
            #fields_def
        }

        impl #builder_name {
            #field_funcs

            fn build(&self) -> Result<#name, Box<dyn Error>> {
                Ok(#name {
                    #build_def
                })
            }
        }

        impl #name {
            #vis fn builder() -> #builder_name {
                #builder_name::default()
            }
        }
    };

    // Hand the output tokens back to the compiler
    expanded.into()
}

fn extract_type_from_option(field_ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath { path, .. }) = &field_ty {
        let segments_str = &path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>()
            .join(":");
        // Concatenate `PathSegment` into a string, compare and take out the `PathSegment` where `Option` is located
        let option_segment = ["Option", "std:option:Option", "core:option:Option"]
            .iter()
            .find(|s| segments_str == *s)
            .and_then(|_| path.segments.last());
        let inner_type = option_segment
            // Take out the generic parameters of the `PathSegment` where `Option` is located
            // If it is not generic, it is not possible to be `Option<T>`, return `None`
            // But this situation may not occur
            .and_then(|path_seg| match &path_seg.arguments {
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) => args.first(),
                _ => None,
            })
            // Take out the type information in the generic parameter
            // If it is not a type, it is not possible to be `Option<T>`, return `None`
            // But this situation may not occur
            .and_then(|generic_arg| match generic_arg {
                syn::GenericArgument::Type(ty) => Some(ty),
                _ => None,
            });

        inner_type
    } else {
        None
    }
}
