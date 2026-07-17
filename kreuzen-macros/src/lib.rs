use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{Ident, Token};

#[derive(Debug, syn_derive::Parse)]
enum Input {
	#[parse(peek = Token![struct])]
	Struct { _struct: Token![struct], ty: syn::Type, body: Body },
	#[parse(peek = Token![enum])]
	Enum {
		_enum: Token![enum],
		ty: syn::Type,
		#[syn(braced)]
		_brace: syn::token::Brace,
		#[syn(in = _brace)]
		#[parse(Punctuated::parse_terminated)]
		variants: Punctuated<Variant, Token![,]>,
	},
}

#[derive(Debug, syn_derive::Parse)]
struct Variant {
	name: Ident,
	body: Body,
	#[parse(parse_opt_alias)]
	alias: Option<(Token![=], syn::LitStr)>,
}

fn parse_opt_alias(inp: ParseStream) -> syn::Result<Option<(Token![=], syn::LitStr)>> {
	if let Some(v) = inp.parse()? {
		Ok(Some((v, inp.parse()?)))
	} else {
		Ok(None)
	}
}

#[derive(Debug, syn_derive::Parse)]
enum Body {
	#[parse(peek = syn::token::Brace)]
	Brace {
		#[syn(braced)]
		_brace: syn::token::Brace,
		#[syn(in = _brace)]
		#[parse(Punctuated::parse_terminated)]
		fields: Punctuated<Field, Token![,]>,
	},
	#[parse(peek = syn::token::Paren)]
	Paren {
		#[syn(parenthesized)]
		_paren: syn::token::Paren,
		#[syn(in = _paren)]
		#[parse(Punctuated::parse_terminated)]
		fields: Punctuated<Field, Token![,]>,
	},
	Empty,
}

#[derive(Debug, syn_derive::Parse)]
struct Field {
	name: Ident,
	star: Option<Token![*]>,
}

#[proc_macro]
pub fn row(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = syn::parse_macro_input!(ts as Input);
	let (ty, print, parse) = match input {
		Input::Struct { ty, body, .. } => {
			let Out { print_fields, prints, parse_fields } = process(body);
			let print = quote! {
				let Self { #print_fields } = self;
				#prints
			};
			let parse = quote! {
				Ok(Self { #parse_fields })
			};
			(ty, print, parse)
		}
		Input::Enum { ty, variants, .. } => {
			let mut variant = Vec::new();
			let mut alias = Vec::new();
			let mut print_fields = Vec::new();
			let mut prints = Vec::new();
			let mut parse_fields = Vec::new();
			for v in variants {
				variant.push(v.name.clone());
				alias.push(match v.alias {
					Some((_, a)) => quote!(#a),
					None => {
						let name = v.name;
						quote!(stringify!(#name))
					}
				});
				let out = process(v.body);
				print_fields.push(out.print_fields);
				prints.push(out.prints);
				parse_fields.push(out.parse_fields);
			}
			let print = quote! {
				match self {
					#(Self::#variant { #print_fields } => {
						ctx.word(#alias);
						#prints
					})*
				}
			};
			let parse = quote! {
				p.alt()
					#(.test_kw(#alias, |p| Ok(Self::#variant { #parse_fields })))*
					.finish()
			};
			(ty, print, parse)
		}
	};

	quote! {
		#[allow(non_shorthand_field_patterns)]
		impl Print for #ty {
			fn print(&self, ctx: &mut Printer) {
				#print
			}
		}
		#[allow(non_shorthand_field_patterns)]
		impl Parse for #ty {
			fn parse(p: &mut Parser) -> Result<Self> {
				#parse
			}
		}
	}
	.into()
}

#[derive(Debug, Default)]
struct Out {
	print_fields: TokenStream,
	prints: TokenStream,
	parse_fields: TokenStream,
}

fn process(body: Body) -> Out {
	let mut out = Out::default();
	match body {
		Body::Brace { fields, .. } => {
			for f in fields {
				process_field(&mut out, f.name.clone().into(), f);
			}
		}
		Body::Paren { fields, .. } => {
			for (i, f) in fields.into_iter().enumerate() {
				process_field(&mut out, i.into(), f);
			}
		}
		Body::Empty => {}
	}
	out
}

fn process_field(out: &mut Out, member: syn::Member, f: Field) {
	let name = f.name;
	if f.star.is_some() {
		out.print_fields.extend(quote! { #member: #name, });
		out.prints.extend(quote! { for __f in #name { __f.print(ctx); } });
		out.parse_fields.extend(quote! { #member: p.parse_many()?, });
	} else {
		out.print_fields.extend(quote! { #member: #name, });
		out.prints.extend(quote! { #name.print(ctx); });
		out.parse_fields.extend(quote! { #member: p.parse()?, });
	}
}
