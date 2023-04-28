use crate::{
  render::painter::Painter,
  widgets::{widget::WidgetError, Widget},
};

#[derive(PartialEq, Eq)]
pub enum Mark {
  Test,
  Spreadsheet,
  Toolbar,
}

// TODO: add pull cache?
pub struct Ring {
  widgets: Vec<(Box<dyn Widget>, Mark)>,
}

impl Ring {
  pub fn new() -> Self {
    Self { widgets: vec![] }
  }

  pub fn push(&mut self, w: Box<dyn Widget>, m: Mark) {
    self.widgets.push((w, m));
  }

  pub fn pull_mut(&mut self, mark: Mark) -> Option<&mut dyn Widget> {
    self
      .widgets
      .iter()
      .position(|(_, m)| *m == mark)
      .map(|i| self.widgets.get_mut(i).unwrap().0.as_mut())
  }

  pub fn for_each<F>(&mut self, mut f: F)
  where
    F: FnMut(&mut dyn Widget) -> (),
  {
    for (w, _) in self.widgets.iter_mut() {
      f(w.as_mut());
    }
  }

  pub fn delete(&mut self, m: Mark) -> bool {
    let len = self.widgets.len();
    self.widgets.retain(|(_, _m)| *_m != m);
    len != self.widgets.len()
  }

  pub fn draw(&mut self, painter: &dyn Painter) -> Vec<WidgetError> {
    let p = self.widgets.iter_mut().try_for_each(|(w, _)| {
      if !w.plotted() {
        unsafe { w.plot(painter) }
      } else {
        Ok(())
      }
    });
    if let Err(e) = p {
      return vec![e];
    }

    self
      .widgets
      .iter()
      .map(|(w, _)| w.draw(painter))
      .filter_map(|r| r.err())
      .collect()
  }
}

#[cfg(test)]
mod tests {
  use crate::{
    render::painter::{MockPainter, Painter},
    widgets::{widget::WidgetError, Widget},
  };

  use super::Ring;

  struct W {
    fail_plot: bool,
    fail_draw: bool,
    plotted: bool,
  }

  impl W {
    fn new(fail_plot: bool, fail_draw: bool, plotted: bool) -> Self {
      Self {
        fail_plot,
        fail_draw,
        plotted,
      }
    }
  }

  impl Widget for W {
    unsafe fn plot(&mut self, _: &dyn Painter) -> Result<(), WidgetError> {
      if self.fail_plot {
        Err(WidgetError::Unspecified("plot failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn draw(&self, _: &dyn Painter) -> Result<(), WidgetError> {
      if self.fail_draw {
        Err(WidgetError::Unspecified("draw failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn catch(&self) -> bool {
      todo!()
    }

    fn plotted(&self) -> bool {
      self.plotted
    }

    fn set_origin(&mut self, _: &crate::render::Point) {
      todo!()
    }

    fn request_plot(&mut self) {
      todo!()
    }
  }

  #[test]
  fn add_rem() {
    let mut r = Ring::new();
    let w = Box::new(W::new(false, false, false));
    r.push(w, super::Mark::Test);
    assert_eq!(r.delete(super::Mark::Test), true);
    assert_eq!(r.delete(super::Mark::Test), false);
  }

  #[test]
  fn error_handling() {
    let p = MockPainter::new();
    let mut r = Ring::new();

    // all good
    let norm = Box::new(W::new(false, false, false));
    r.push(norm, super::Mark::Test);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 0);

    // drawing failed
    let fail_draw = Box::new(W::new(false, true, false));
    r.push(fail_draw, super::Mark::Test);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("draw failed".to_owned())
    );

    // drawing doesn't fail fast
    let fail_draw = Box::new(W::new(false, true, false));
    r.push(fail_draw, super::Mark::Test);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 2);

    // plotting failed, drawing never tried
    let fail_plot = Box::new(W::new(true, false, false));
    r.push(fail_plot, super::Mark::Test);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("plot failed".to_owned())
    );

    // plotting fails fast
    let fail_plot = Box::new(W::new(true, false, false));
    r.push(fail_plot, super::Mark::Test);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
  }

  #[test]
  fn checks_for_plotting() {
    let p = MockPainter::new();
    let mut r = Ring::new();
    let norm = Box::new(W::new(false, false, false));
    let plotted = Box::new(W::new(true, false, true));
    r.push(norm, super::Mark::Test);
    r.push(plotted, super::Mark::Test);
    assert_eq!(r.draw(&p).len(), 0);
  }
}
