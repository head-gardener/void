use std::{
  cell::{RefCell, RefMut},
  rc::Rc,
};

use crate::{
  render::{painter::Painter, Point},
  widgets::traits::{CallbackResult, ClickSink, Parent, Widget, WidgetError},
};

#[derive(PartialEq, Eq)]
pub enum Mark {
  #[cfg(test)]
  Test,
  None,
  Window,
  Spreadsheet,
  Toolbar,
  ToolbarDropdown,
}

pub trait RingMember {
  fn push_to_ring(&self, ring: &mut Ring);
}

// TODO: add pull cache?
pub struct Ring {
  widgets: Vec<(Rc<RefCell<dyn Widget>>, Mark, Mark, usize)>,
  clickable: Vec<(Rc<RefCell<dyn ClickSink>>, Mark)>,
  parents: Vec<(Rc<RefCell<dyn Parent>>, Mark)>,
}

impl Ring {
  pub fn new() -> Self {
    Self {
      widgets: vec![],
      clickable: vec![],
      parents: vec![],
    }
  }

  pub fn push(
    &mut self,
    w: Rc<RefCell<dyn Widget>>,
    m: Mark,
    parent: Mark,
    n: usize,
  ) {
    assert!(m != parent);
    self.widgets.push((w, m, parent, n));
  }

  pub fn push_clickable(&mut self, w: Rc<RefCell<dyn ClickSink>>, m: Mark) {
    self.clickable.push((w, m));
  }

  pub fn push_parent(&mut self, w: Rc<RefCell<dyn Parent>>, m: Mark) {
    self.parents.push((w, m));
  }

  pub fn pull(&self, mark: &Mark) -> Option<Rc<RefCell<dyn Widget>>> {
    self
      .widgets
      .iter()
      .position(|(_, m, _, _)| *m == *mark)
      .map(|i| self.widgets.get(i).unwrap().0.clone())
  }

  pub fn pull_parent(&self, mark: &Mark) -> Option<Rc<RefCell<dyn Parent>>> {
    self
      .parents
      .iter()
      .position(|(_, m)| *m == *mark)
      .map(|i| self.parents.get(i).unwrap().0.clone())
  }

  pub fn for_each<F>(&mut self, mut f: F)
  where
    F: FnMut(RefMut<dyn Widget>) -> (),
  {
    for (w, _, _, _) in self.widgets.iter_mut() {
      f(w.borrow_mut());
    }
  }

  pub fn delete(&mut self, m: Mark) -> bool {
    let len = self.widgets.len();
    self.widgets.retain(|(_, _m, _, _)| *_m != m);
    self.clickable.retain(|(_, _m)| *_m != m);
    len != self.widgets.len()
  }

  pub fn draw(&mut self, painter: &Painter) -> Vec<WidgetError> {
    let p = self.widgets.iter().try_for_each(|(w, _, p, n)| {
      let mut w = w.borrow_mut();
      if !w.plotted() {
        if *p != Mark::None {
          let o = self
            .pull_parent(p)
            .ok_or(WidgetError::Unspecified("Invalid parent mark".to_owned()))?
            .borrow()
            .nth_child(*n)
            .ok_or(WidgetError::Unspecified(format!(
              "Child out of bounds: {}",
              n
            )))?;
          w.set_origin(&o);
        }
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
      .map(|(w, _, _, _)| {
        let w = w.borrow_mut();
        w.draw(painter)
      })
      .filter_map(|r| r.err())
      .collect()
  }

  pub fn catch_click(&mut self, painter: &Painter, p: Point) {
    let mut r = CallbackResult::Skip;

    for (w, _) in self.clickable.iter() {
      match w.borrow().handle_click(painter, p) {
        CallbackResult::Skip => continue,
        res => {
          r = res;
          break;
        }
      }
    }

    match r {
      CallbackResult::Skip => {}
      CallbackResult::None => {}

      CallbackResult::Error(e) => {
        println!("Callback failed: {}", e);
      }
      CallbackResult::Push(x) => x.push_to_ring(self),
      CallbackResult::Modify(_, _) => todo!(),
    }
  }
}

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use crate::{
    render::painter::Painter,
    widgets::traits::{widget::WidgetError, Widget},
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

    pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
      let rc = Rc::new(RefCell::new(self));
      ring.push(
        rc,
        crate::logic::ring::Mark::Test,
        crate::logic::ring::Mark::None,
        0,
      );
    }
  }

  impl Widget for W {
    unsafe fn plot(&mut self, _: &Painter) -> Result<(), WidgetError> {
      if self.fail_plot {
        Err(WidgetError::Unspecified("plot failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn draw(&self, _: &Painter) -> Result<(), WidgetError> {
      if self.fail_draw {
        Err(WidgetError::Unspecified("draw failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn plotted(&self) -> bool {
      self.plotted
    }

    fn set_origin(&mut self, _: &crate::render::Origin) {
      todo!()
    }

    fn request_plot(&mut self) {
      todo!()
    }
  }

  #[test]
  fn add_rem() {
    let mut r = Ring::new();
    let w = W::new(false, false, false);
    w.push_to_ring(&mut r);

    assert_eq!(r.delete(super::Mark::Test), true);
    assert_eq!(r.delete(super::Mark::Test), false);
  }

  #[test]
  fn error_handling() {
    let p = Painter::new(0, 0);
    let mut r = Ring::new();

    // all good
    let norm = W::new(false, false, false);
    norm.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 0);

    // drawing failed
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("draw failed".to_owned())
    );

    // drawing doesn't fail fast
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 2);

    // plotting failed, drawing never tried
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("plot failed".to_owned())
    );

    // plotting fails fast
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
  }

  #[test]
  fn checks_for_plotting() {
    let p = Painter::new(0, 0);
    let mut r = Ring::new();
    let norm = W::new(false, false, false);
    let plotted = W::new(true, false, true);
    norm.push_to_ring(&mut r);
    plotted.push_to_ring(&mut r);
    assert_eq!(r.draw(&p).len(), 0);
  }
}
