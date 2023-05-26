use serde::Serialize;

pub type Tag = u32;

/// An entry in a spreadsheet. Implement with `Entry` derive macro.
/// When doing so, prefix all data field with `d_` and give them `&'a str` type.
/// Those fields will be used when displaying data.
pub trait Record: Recordable {
  const N_FIELDS: usize;

  fn datatypes() -> Vec<Datatype>;
  fn uid(&self) -> &i64;
  fn set_uid(&mut self, u: i64);

  /// Set nth field from `str`.
  fn set_nth_str(&mut self, n: usize, val: &str);
  /// Set nth field from `i64`.
  fn set_nth_raw(&mut self, n: usize, val: Data);

  /// Get nth field.
  fn get_nth(&self, n: usize) -> Option<Data>;
}

pub trait Recordable:
  Serialize + Header + Clone + Send + Sync + Default
{
}

pub trait Header {
  fn header() -> Vec<&'static str>;
}

pub enum Datatype {
  String,
  Integer,
  FKey(Tag, usize),
}

pub type FKey = Option<i64>;

static DEF_STR: Data = Data::String(String::new());
static DEF_INT: Data = Data::I64(0);
static DEF_FKEY: Data = Data::FKey(None);

impl Datatype {
  pub fn default(&self) -> &'static Data {
    match self {
      Datatype::String => &DEF_STR,
      Datatype::Integer => &DEF_INT,
      Datatype::FKey(_, _) => &DEF_FKEY,
    }
  }

  /// Returns `true` if the datatype is [`FKey`].
  ///
  /// [`FKey`]: Datatype::FKey
  #[must_use]
  pub fn is_fkey(&self) -> bool {
    matches!(self, Self::FKey(..))
  }
}

#[derive(Debug, Clone)]
pub enum Data {
  String(String),
  I64(i64),
  FKey(Option<i64>),
}

impl ToString for Data {
  fn to_string(&self) -> String {
    match self {
      Data::String(s) => s.to_string(),
      Data::I64(i) => i.to_string(),
      Data::FKey(None) => "Select...".to_string(),
      Data::FKey(Some(k)) => k.to_string(),
    }
  }
}
