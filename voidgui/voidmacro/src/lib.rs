#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use proc_macro::TokenStream;
use syn::{
  parse::{Parse, ParseStream},
  Data, DeriveInput, Field, Ident, Type,
};

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

struct FKeyParams(syn::Ident, syn::Ident);
impl Parse for FKeyParams {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let content;
    syn::parenthesized!(content in input);
    let tag = content.parse()?;
    content.parse::<Token![,]>()?;
    let field = content.parse()?;
    Ok(FKeyParams(tag, field))
  }
}

#[proc_macro_derive(Record, attributes(fkey))]
pub fn derive_record(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  //   let attrib = input
  //     .attrs
  //     .iter()
  //     .find(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == "fkey")
  //     .expect("my_trait attribute required for deriving MyTrait!");

  //   let parameters: FKeyParams =
  //     attrib.parse.expect("Invalid my_trait attribute!");

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
  let ids1 = ids.clone();
  let ids2 = ids.clone();
  let ids3 = ids.clone();

  let dt = fields.iter().map(|f| match &f.ty {
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(String)) => {
      quote!(voidgui::data::Datatype::String)
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(i64)) => {
      quote!(voidgui::data::Datatype::Integer)
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(FKey)) => {
      quote!(voidgui::data::Datatype::FKey(1, 0))
    }
    Type::Path(p) => {
      panic!(
        "Unexpected path identifier {}",
        p.path.segments[0].ident.to_string()
      )
    }
    _ => panic!(
      "Unexpected type for field {:?}",
      f.ident.as_ref().map(|i| i.to_string())
    ),
  });
  let ds = fields.iter().zip(ids.clone()).map(|(f, id)| match &f.ty {
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(String)) => {
      quote!(voidgui::data::Data::String(self.#id.clone()))
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(i64)) => {
      quote!(voidgui::data::Data::I64(self.#id))
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(FKey)) => {
      quote!(voidgui::data::Data::FKey(self.#id))
    }
    _ => panic!(
      "Unexpected type for field {:?}",
      f.ident.as_ref().map(|i| i.to_string())
    ),
  });
  let n_fields = fields.len();

  let enm = 0..n_fields;
  let enm1 = enm.clone();
  let enm2 = enm.clone();
  let conv_str = fields.iter().map(|f| match &f.ty {
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(String)) => {
      quote!(val.to_string())
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(i64)) => {
      quote!(val.parse().unwrap())
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(FKey)) => {
      quote!(panic!())
      // quote!(Some(val.parse().unwrap()))
    }
    _ => panic!(
      "Unexpected type for field {:?}",
      f.ident.as_ref().map(|i| i.to_string())
    ),
  });
  let conv_dat = fields.iter().map(|f| match &f.ty {
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(String)) => {
      quote!(match val {
        voidgui::data::Data::String(s) => s,
        _ => panic!(),
      })
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(i64)) => {
      quote!(match val {
        voidgui::data::Data::I64(s) => s,
        _ => panic!(),
      })
    }
    Type::Path(p) if p.path.is_ident::<Ident>(parse_quote!(FKey)) => {
      quote!(match val {
        voidgui::data::Data::FKey(u) => u,
        _ => panic!(),
      })
    }
    _ => panic!(
      "Unexpected type for field {:?}",
      f.ident.as_ref().map(|i| i.to_string())
    ),
  });

  let msg = "{} ({}) out of range for ".to_string() + &name.to_string();

  // TODO: replace Default::default() with concrete values to match
  // datatype defaults.
  let expanded = quote! {
    impl #gen voidgui::data::Recordable #gen for #name #gen {}

    impl #gen Default #gen for #name #gen {
      fn default() -> Self {
        Self {
          uid: Default::default(),
          #(#ids3: Default::default()),*
        }
      }
    }

    impl #gen voidgui::data::Record #gen for #name #gen {
      const N_FIELDS: usize = #n_fields;

      fn uid(&self) -> &i64 {
        &self.uid
      }

      fn set_uid(&mut self, uid: i64) {
        self.uid = uid;
      }

      fn datatypes() -> Vec<voidgui::data::Datatype> {
        vec![#(#dt),*]
      }

      fn set_nth_str(&mut self, n: usize, val: &str) {
        match n {
          #(#enm => self.#ids1 = #conv_str,)*
          _ => panic!(#msg, n, val)
        }
      }

      fn set_nth_raw(&mut self, n: usize, val: Data) {
        match n {
          #(#enm1 => self.#ids2 = #conv_dat,)*
          _ =>
            panic!("bad n {}", n)
        }
      }

      fn get_nth(&self, n: usize) -> Option<Data> {
        match n {
          #(#enm2 => Some(#ds),)*
          _ => None
        }
      }
    }
  };

  TokenStream::from(expanded)
}
