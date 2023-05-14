#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Ident};

// MENU

#[proc_macro_derive(Menu)]
pub fn derive_menu(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let gen = input.generics;

  let expanded = quote! {
    impl #gen Widget for #name #gen {
      fn plotted(&self) -> bool {
        self.table.plotted()
      }
      fn set_origin(&mut self, origin: &Origin) {
        self.table.set_origin(origin.clone());
      }
      fn request_plot(&mut self) {
        self.table.request_plot();
      }
    }
  };

  TokenStream::from(expanded)
}

#[proc_macro_derive(DrawableMenu)]
pub fn derive_draw_menu(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let mut gen = input.generics;
  gen.type_params_mut().for_each(|x| {
    x.bounds
      .push(syn::TypeParamBound::Trait(parse_quote!(Sync)));
    x.bounds
      .push(syn::TypeParamBound::Trait(parse_quote!(Send)));
    x.bounds
      .push(syn::TypeParamBound::Lifetime(parse_quote!('static)));
  });
  let (impl_gen, ty_gen, where_clause) = gen.split_for_impl();

  let expanded = quote! {
    impl #impl_gen Drawable for #name #ty_gen #where_clause {
      fn plot(
        &mut self,
        desc: RwLockReadGuard<Description>,
        mut feed: DroneFeed
      ) -> Result<(), widgets::Error>
      {
        self.table.plot(&desc, &mut feed)
      }
      unsafe fn draw(&mut self, mut feed: DroneFeed)
        -> Result<(), widgets::Error>
      {
        self.table.draw(&mut feed)
      }
    }
  };

  TokenStream::from(expanded)
}

#[proc_macro_derive(ClickableMenu)]
pub fn derive_clickable_menu(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let gen = input.generics;

  let expanded = quote! {
    impl #gen Clickable for #name #gen {
      #[inline]
      fn click_area(&self) -> Option<Area> {
        self.table.area()
      }
    }
  };

  TokenStream::from(expanded)
}

// SPREADSHEET

#[proc_macro_derive(Entry)]
pub fn derive_entry(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let gen = input.generics;

  let fields: Vec<Ident> = match input.data {
    Data::Struct(d) => d
      .fields
      .iter()
      .filter_map(|f| f.ident.to_owned())
      .filter(|i| {
          i.to_string().starts_with("d_")
      })
      .collect(),
    _ => todo!(),
  };
  let n_fields = fields.len();

  let expanded = quote! {
    impl #gen voidgui::widgets::spreadsheet::Entry #gen for #name #gen {
      const N_FIELDS: usize = #n_fields;

      fn fields(self) -> Vec<&'a str> {
        vec![#(self.#fields),*]
      }

      fn uuid(&self) -> &u64 {
        &self.uuid
      }
    }
  };

  TokenStream::from(expanded)
}
