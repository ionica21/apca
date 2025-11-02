use crate::Str;
use chrono::NaiveDate;
use num_decimal::Num;
use serde::{Deserialize, Serialize, Serializer};
use std::fmt::Result as FmtResult;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::str::FromStr;
use uuid::Error as UuidError;
use uuid::Uuid;

/// An ID uniquely identifying an options contract.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Id(pub Uuid);

impl Deref for Id {
  type Target = Uuid;

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

/// The status an options contract can have.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum Status {
  /// The asset is active.
  #[serde(rename = "active")]
  Active,
  /// The asset is inactive.
  #[serde(rename = "inactive")]
  Inactive,
  /// Any other asset status that we have not accounted for.
  ///
  /// Note that having any such unknown asset class should be considered
  /// a bug.
  #[doc(hidden)]
  #[serde(other, rename(serialize = "unknown"))]
  Unknown,
}

impl AsRef<str> for Status {
  #[inline]
  fn as_ref(&self) -> &'static str {
    match *self {
      Status::Active => "active",
      Status::Inactive => "inactive",
      Status::Unknown => "unknown",
    }
  }
}

impl Default for Status {
  #[inline]
  fn default() -> Self {
    Self::Active
  }
}

/// An enumeration of all possible symbol parsing errors.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ParseSymbolError {
  /// The symbol contains an invalid character.
  InvalidSymbol(char),
  /// The ID could not be parsed.
  InvalidId(UuidError),
}

impl Display for ParseSymbolError {
  fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
    match self {
      Self::InvalidSymbol(c) => write!(fmt, "the symbol contains an invalid character ('{c}')"),
      Self::InvalidId(err) => write!(fmt, "failed to parse asset ID: {err}"),
    }
  }
}

/// A symbol and the various ways to represent it.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(try_from = "&str")]
#[non_exhaustive]
pub enum Symbol {
  /// The options contract symbol.
  Sym(String),
  /// An asset as described by an ID.
  Id(Id),
}

impl From<Id> for Symbol {
  #[inline]
  fn from(symbol: Id) -> Self {
    Self::Id(symbol)
  }
}

impl TryFrom<&str> for Symbol {
  type Error = ParseSymbolError;

  fn try_from(other: &str) -> Result<Self, Self::Error> {
    Symbol::from_str(other)
  }
}

impl FromStr for Symbol {
  type Err = ParseSymbolError;

  fn from_str(sym: &str) -> Result<Self, Self::Err> {
    let sym = if let Ok(id) = Uuid::parse_str(sym) {
      Self::Id(Id(id))
    } else {
      let invalid = sym.as_bytes().iter().try_fold((), |(), c| {
        // Alphanumeric is valid. Alphabet characters must be uppercase.
        if !c.is_ascii_alphanumeric() || (c.is_ascii_alphabetic() && !c.is_ascii_uppercase()) {
          Err(*c as char)
        } else {
          Ok(())
        }
      });

      if let Err(c) = invalid {
        return Err(ParseSymbolError::InvalidSymbol(c));
      }
      Self::Sym((*sym).to_string())
    };
    Ok(sym)
  }
}

/// An enumeration of option types.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum OptionType {
  /// Call option.
  #[serde(rename = "call")]
  Call,
  /// Put option.
  #[serde(rename = "put")]
  Put,
}

impl AsRef<str> for OptionType {
  fn as_ref(&self) -> &'static str {
    match *self {
      OptionType::Call => "call",
      OptionType::Put => "put",
    }
  }
}

impl FromStr for OptionType {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s == OptionType::Call.as_ref() {
      Ok(OptionType::Call)
    } else if s == OptionType::Put.as_ref() {
      Ok(OptionType::Put)
    } else {
      Err(())
    }
  }
}

/// An enumeration of option styles.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum OptionStyle {
  /// American style option.
  #[serde(rename = "american")]
  American,
  /// European style option.
  #[serde(rename = "european")]
  European,
}

impl AsRef<str> for OptionStyle {
  fn as_ref(&self) -> &'static str {
    match *self {
      OptionStyle::American => "american",
      OptionStyle::European => "european",
    }
  }
}

impl FromStr for OptionStyle {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s == OptionStyle::American.as_ref() {
      Ok(OptionStyle::American)
    } else if s == OptionStyle::European.as_ref() {
      Ok(OptionStyle::European)
    } else {
      Err(())
    }
  }
}

impl Display for Symbol {
  fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
    match self {
      Self::Sym(sym) => fmt.write_str(sym),
      Self::Id(id) => write!(fmt, "{}", id.as_hyphenated()),
    }
  }
}

impl Serialize for Symbol {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serializer.serialize_str(&self.to_string())
  }
}

/// The representation of an options contract as used by Alpaca.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Contract {
  /// The contract's ID.
  #[serde(rename = "id")]
  pub id: Id,
  /// The contract's symbol.
  #[serde(rename = "symbol")]
  pub symbol: String,
  /// The contract's name.
  #[serde(rename = "name")]
  pub name: String,
  /// The contract's status.
  #[serde(rename = "status")]
  pub status: Status,
  /// Whether the contract is tradable on Alpaca or not.
  #[serde(rename = "tradable")]
  pub tradable: bool,
  /// Expiration date of the contract.
  #[serde(rename = "expiration_date")]
  pub expiration_date: NaiveDate,
  /// Root symbol of the contract.
  #[serde(rename = "root_symbol")]
  pub root_symbol: String,
  /// Underlying symbol of the contract.
  #[serde(rename = "underlying_symbol")]
  pub underlying_symbol: String,
  /// Asset id of the underlying asset of the contract.
  #[serde(rename = "underlying_asset_id")]
  pub underlying_asset_id: Id,
  /// Type of the option contract.
  #[serde(rename = "type")]
  pub option_type: OptionType,
  /// Style of the option contract.
  #[serde(rename = "style")]
  pub style: OptionStyle,
  /// Strike price of the option contract.
  #[serde(rename = "strike_price")]
  pub strike_price: Num,
  /// Multiplier of the option contract used to calculate the price.
  #[serde(rename = "multiplier")]
  pub multiplier: Num,
  /// Size of the option contract representing the number of underlying shares to be delivered.
  #[serde(rename = "size")]
  pub size: Num,
  /// Open interest of the options contract.
  #[serde(rename = "open_interest")]
  pub open_interest: Option<Num>,
  /// Date from which the open_interest value has been obtained for the options contract.
  #[serde(rename = "open_interest_date")]
  pub open_interest_date: Option<NaiveDate>,
  /// Close price of the options contract.
  #[serde(rename = "close_price")]
  pub close_price: Option<Num>,
  /// Date from which the close_price value has been obtained for the options contract.
  #[serde(rename = "close_price_date")]
  pub close_price_date: Option<NaiveDate>,
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}

Endpoint! {
  /// The representation of a GET request to the /v2/options/{symbol} endpoint.
  pub Get(Symbol),
  Ok => Contract, [
    /// The contract object for the given symbol was retrieved successfully.
    /* 200 */ OK,
  ],
  Err => GetError, [
    /// No contract was found for the given symbol.
    /* 404 */ NOT_FOUND => NotFound,
  ]

  #[inline]
  fn path(input: &Self::Input) -> Str {
    format!("/v2/options/contracts/{input}").into()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use serde_json::from_str as from_json;
  use serde_json::to_string as to_json;

  use test_log::test;

  use uuid::Uuid;

  use crate::api_info::ApiInfo;
  use crate::Client;

  /// Verify that we can parse various symbols.
  #[test]
  fn parse_symbol() {
    let id = "b0b6dd9d-8b9b-48a9-ba46-b9d54906e415";
    assert_eq!(
      Symbol::from_str(id).unwrap(),
      Symbol::Id(Id(Uuid::parse_str(id).unwrap())),
    );

    assert_eq!(Symbol::from_str("SPY").unwrap(), Symbol::Sym("SPY".into()));
    assert_eq!(
      Symbol::from_str("Z%&Y"),
      Err(ParseSymbolError::InvalidSymbol('%')),
    );
  }

  /// Make sure that we can serialize and deserialize a symbol.
  #[test]
  fn serialize_deserialize_symbol() {
    let symbol = Symbol::Sym("AAPL".to_string());
    let json = to_json(&symbol).unwrap();
    assert_eq!(json, r#""AAPL""#);
    assert_eq!(from_json::<Symbol>(&json).unwrap(), symbol);

    let id = Id(Uuid::parse_str("b0b6dd9d-8b9b-48a9-ba46-b9d54906e415").unwrap());
    let symbol = Symbol::Id(id);
    let json = to_json(&symbol).unwrap();
    assert_eq!(json, r#""b0b6dd9d-8b9b-48a9-ba46-b9d54906e415""#);
    assert_eq!(from_json::<Symbol>(&json).unwrap(), symbol);
  }

  /// Check that we can parse a reference contract object.
  #[test]
  fn parse_reference_contract() {
    let response = r#"{
      "id": "d46be4ed-2ce2-4945-9965-1b520dc60c5b",
      "symbol": "AAPL251107C00110000",
      "name": "AAPL Nov 07 2025 110 Call",
      "status": "active",
      "tradable": true,
      "expiration_date": "2025-11-07",
      "root_symbol": "AAPL",
      "underlying_symbol": "AAPL",
      "underlying_asset_id": "b0b6dd9d-8b9b-48a9-ba46-b9d54906e415",
      "type": "call",
      "style": "american",
      "strike_price": "110",
      "multiplier": "100",
      "size": "100",
      "open_interest": "6",
      "open_interest_date": "2025-10-30",
      "close_price": "160.35",
      "close_price_date": "2025-10-31",
      "ppind": true
    }"#;

    let id = Id(Uuid::parse_str("d46be4ed-2ce2-4945-9965-1b520dc60c5b").unwrap());
    let underlying_id = Id(Uuid::parse_str("b0b6dd9d-8b9b-48a9-ba46-b9d54906e415").unwrap());
    let contract = from_json::<Contract>(response).unwrap();
    assert_eq!(contract.id, id);
    assert_eq!(contract.symbol, "AAPL251107C00110000");
    assert_eq!(contract.name, "AAPL Nov 07 2025 110 Call");
    assert_eq!(contract.status, Status::Active);
    assert!(contract.tradable);
    assert_eq!(
      contract.expiration_date,
      NaiveDate::parse_from_str("2025-11-07", "%Y-%m-%d").unwrap()
    );
    assert_eq!(contract.root_symbol, "AAPL");
    assert_eq!(contract.underlying_symbol, "AAPL");
    assert_eq!(contract.underlying_asset_id, underlying_id);
    assert_eq!(contract.option_type, OptionType::Call);
    assert_eq!(contract.style, OptionStyle::American);
    assert_eq!(contract.strike_price, Num::from_str("110").unwrap());
    assert_eq!(contract.multiplier, Num::from_str("100").unwrap());
    assert_eq!(contract.size, Num::from_str("100").unwrap());
    assert_eq!(contract.open_interest, Some(Num::from_str("6").unwrap()));
    assert_eq!(
      contract.open_interest_date,
      Some(NaiveDate::parse_from_str("2025-10-30", "%Y-%m-%d").unwrap())
    );
    assert_eq!(contract.close_price, Some(Num::from_str("160.35").unwrap()));
    assert_eq!(
      contract.close_price_date,
      Some(NaiveDate::parse_from_str("2025-10-31", "%Y-%m-%d").unwrap())
    );
  }

  /// Check that we can parse a reference contract object with None values.
  #[test]
  fn parse_reference_contract_with_none_values() {
    let response = r#"{
      "id": "125f8a90-8494-469c-a1d2-b1d58916da6c",
      "symbol": "AA251107C00020000",
      "name": "AA Nov 07 2025 20 Call",
      "status": "active",
      "tradable": true,
      "expiration_date": "2025-11-07",
      "root_symbol": "AA",
      "underlying_symbol": "AA",
      "underlying_asset_id": "3ca0202f-01f4-41a0-bb0c-c8864e767ebd",
      "type": "call",
      "style": "american",
      "strike_price": "20",
      "multiplier": "100",
      "size": "100",
      "open_interest": null,
      "open_interest_date": null,
      "close_price": null,
      "close_price_date": null,
      "ppind": true
    }"#;

    let id = Id(Uuid::parse_str("125f8a90-8494-469c-a1d2-b1d58916da6c").unwrap());
    let underlying_id = Id(Uuid::parse_str("3ca0202f-01f4-41a0-bb0c-c8864e767ebd").unwrap());
    let contract = from_json::<Contract>(response).unwrap();
    assert_eq!(contract.id, id);
    assert_eq!(contract.symbol, "AA251107C00020000");
    assert_eq!(contract.name, "AA Nov 07 2025 20 Call");
    assert_eq!(contract.status, Status::Active);
    assert!(contract.tradable);
    assert_eq!(
      contract.expiration_date,
      NaiveDate::parse_from_str("2025-11-07", "%Y-%m-%d").unwrap()
    );
    assert_eq!(contract.root_symbol, "AA");
    assert_eq!(contract.underlying_symbol, "AA");
    assert_eq!(contract.underlying_asset_id, underlying_id);
    assert_eq!(contract.option_type, OptionType::Call);
    assert_eq!(contract.style, OptionStyle::American);
    assert_eq!(contract.strike_price, Num::from_str("20").unwrap());
    assert_eq!(contract.multiplier, Num::from_str("100").unwrap());
    assert_eq!(contract.size, Num::from_str("100").unwrap());
    assert_eq!(contract.open_interest, None);
    assert_eq!(contract.open_interest_date, None);
    assert_eq!(contract.close_price, None);
    assert_eq!(contract.close_price_date, None);
  }

  /// Check that we can serialize and deserialize a `Contract` object.
  #[test(tokio::test)]
  async fn serialize_deserialize_contract() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let contract = client
      .issue::<Get>(&Symbol::try_from("AAPL251107C00110000").unwrap())
      .await
      .unwrap();

    let json = to_json(&contract).unwrap();
    assert_eq!(from_json::<Contract>(&json).unwrap(), contract);
  }

  /// Check that we can create a `Symbol` from an `Id`.
  #[test]
  fn symbol_from_id() {
    let id = Id(Uuid::parse_str("904837e3-3b76-47ec-b432-046db621571b").unwrap());
    let symbol = Symbol::from(id);

    assert_eq!(symbol, Symbol::Id(id))
  }

  /// Check that we can retrieve information about a contract.
  #[test(tokio::test)]
  async fn retrieve_contract() {
    async fn test(symbol: Symbol) {
      let api_info = ApiInfo::from_env().unwrap();
      let client = Client::new(api_info);
      let contract = client.issue::<Get>(&symbol).await.unwrap();

      // The AAPL AAPL Nov 07 2025 110 Call, retrieved out-of-band.
      let id = Id(Uuid::parse_str("d46be4ed-2ce2-4945-9965-1b520dc60c5b").unwrap());
      assert_eq!(contract.id, id);
      assert_eq!(contract.symbol, "AAPL251107C00110000");
      assert_eq!(contract.underlying_symbol, "AAPL");
      assert_eq!(contract.strike_price, Num::from_str("110").unwrap());
      assert_eq!(
        contract.expiration_date,
        NaiveDate::parse_from_str("2025-11-07", "%Y-%m-%d").unwrap()
      );
    }

    let symbols = [
      Symbol::Sym("AAPL251107C00110000".to_string()),
      Symbol::Id(Id(
        Uuid::parse_str("d46be4ed-2ce2-4945-9965-1b520dc60c5b").unwrap(),
      )),
    ];

    for symbol in symbols.iter().cloned() {
      test(symbol).await;
    }
  }
}
