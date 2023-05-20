#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Field, Ident, Type};

// MENU

#[proc_macro_derive(Menu)]
pub fn derive_menu(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let gen = input.generics;

  let origin = match input.data {
    Data::Struct(d) => d
      .fields
      .iter()
      .filter_map(|f| f.ident.to_owned())
      .find(|i| i == "scroll")
      .map_or(quote!(origin.clone()), |_| {
        quote!(origin.clone().scroll(self.scroll))
      }),
    _ => todo!(),
  };

  let expanded = quote! {
    impl #gen Widget for #name #gen {
      fn plotted(&self) -> bool {
        self.table.plotted()
      }
      fn set_origin(&mut self, origin: &Origin) {
        self.table.set_origin(#origin);
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
      fn size(&mut self) -> Size {
        self.table.size()
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

#[proc_macro_derive(Record)]
pub fn derive_entry(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;
  let gen = input.generics;

  let fields: Vec<Field> = match input.data {
    Data::Struct(d) => d
      .fields
      .iter()
      .filter(|f| {
        f.ident
          .as_ref()
          .map(|i| i.to_string().starts_with("d_"))
          .unwrap_or(false)
      })
      .cloned()
      .collect(),
    _ => todo!(),
  };
  let ids = fields.iter().map(|f| f.ident.as_ref().unwrap());
  let dt = fields.iter().map(|f| match &f.ty {
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(String)) => {
      quote!(voidgui::widgets::spreadsheet::Datatype::String)
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(i64)) => {
      quote!(voidgui::widgets::spreadsheet::Datatype::Integer)
    }
    _ => panic!(
      "Unexpected type for field {:?}",
      f.ident.as_ref().map(|i| i.to_string())
    ),
  });
  let n_fields = fields.len();

  let expanded = quote! {
    impl #gen voidgui::widgets::spreadsheet::Record #gen for #name #gen {
      const N_FIELDS: usize = #n_fields;

      fn fields<'a>(&'a self) -> Vec<&'a dyn voidgui::widgets::spreadsheet::Data> {
        vec![#(&self.#ids),*]
      }

      fn uid(&self) -> &i64 {
        &self.uid
      }

      fn datatypes() -> Vec<voidgui::widgets::spreadsheet::Datatype> {
        vec![#(#dt),*]
      }
    }
  };

  TokenStream::from(expanded)
}
