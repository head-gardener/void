#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use proc_macro::TokenStream;
use syn::DeriveInput;

#[proc_macro_derive(Menu)]
pub fn derive_menu(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let name = input.ident;

  let expanded = quote! {
    impl #name {
      unsafe fn make_table(
        painter: &Painter,
        or: Orientation,
        items: &[&str],
      ) -> Result<Self, WidgetError> {
        let (r, c) = match or {
          Orientation::Vertical => (1, items.len()),
          Orientation::Horizontal => (items.len(), 1),
        };

        let mut table = TextTable::from_text(
          painter,
          r,
          c,
          &items
            .iter()
            // TODO: don't do this shit
            .map(|s| Box::new(s.to_string()))
            .collect::<Vec<Box<String>>>(),
        )?;
        table.commit();

        Ok(Self { table })
      }
    }

    impl Widget for #name {
      unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError> {
        self.table.set_origin(Point::new(
          painter.window_area().width - self.table.constr().width,
          0,
        ));
        self.table.plot(painter)
      }

      fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
        self.table.draw(painter)
      }

      fn plotted(&self) -> bool {
        self.table.plotted()
      }

      fn set_origin(&mut self, origin: &Point) {
        self.table.set_origin(origin.clone());
      }

      fn request_plot(&mut self) {
        self.table.request_plot();
      }
    }

    impl Clickable for #name {
      fn click_area(&self) -> Option<crate::render::Area> {
        self.table.area()
      }
    }

    impl ClickableWidget for #name {}
  };

  TokenStream::from(expanded)
}
