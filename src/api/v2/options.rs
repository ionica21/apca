use crate::api::v2::option::{Contract, Status};
use crate::util::vec_from_str;
use crate::Str;
use serde::{Deserialize, Serialize};
use serde_urlencoded::to_string as to_query;

/// A GET request to be made to the /v2/options/contracts endpoint.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ListReq {
  /// Comma-separated list of stock symbols for which to fetch options contracts.
  #[serde(rename = "underlying_symbols", skip_serializing_if = "Option::is_none")]
  pub underlying_symbols: Option<Str>,
  /// The maximum number of contracts to be returned .
  ///
  /// It can be between 1 and 10000. Defaults to 100 if the provided value is None.
  #[serde(rename = "limit", skip_serializing_if = "Option::is_none")]
  pub limit: Option<usize>,
  /// The status of options contracts to include in the response.
  #[serde(rename = "status")]
  pub status: Status,
  /// If provided we will pass a page token to continue where we left off.
  #[serde(rename = "page_token", skip_serializing_if = "Option::is_none")]
  pub page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}

/// A helper for initializing [`ListReq`] objects.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ListReqInit<S> {
  /// See `ListReq::underlying_symbols`.
  pub underlying_symbols: Option<Vec<S>>,
  /// See `ListReq::limit`.
  pub limit: Option<usize>,
  /// See `ListReq::status`.
  pub status: Status,
}

impl<S> ListReqInit<S>
where
  S: Into<String>,
{
  /// Create a [`ListReq`] from a `ListReqInit`.
  #[inline]
  pub fn init(self, page_token: Option<String>) -> ListReq {
    let underlying_symbols = match self.underlying_symbols {
      Some(underlying_symbols) => Some(Str::Owned(
        underlying_symbols
          .into_iter()
          .map(Into::into)
          .collect::<Vec<String>>()
          .join(","),
      )),
      None => None,
    };
    ListReq {
      underlying_symbols,
      limit: self.limit,
      status: self.status,
      page_token,
      _non_exhaustive: (),
    }
  }
}

/// A collection of options contracts as returned by the API. This is one page of contracts.
#[derive(Debug, Deserialize, Eq, PartialEq)]
pub struct Contracts {
  /// The list of returned option contracts.
  #[serde(rename = "option_contracts", deserialize_with = "vec_from_str")]
  pub option_contracts: Vec<Contract>,
  /// The token to provide to a request to get the next page of contracts for this request.
  #[serde(rename = "next_page_token")]
  pub next_page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}

Endpoint! {
  /// The representation of a GET request to the /v2/options/contracts endpoint.
  pub List(ListReq),
  Ok => Contracts, [
    /// The list of contracts was retrieved successfully.
    /* 200 */ OK,
  ],
  Err => ListError, []

  #[inline]
  fn path(_input: &Self::Input) -> Str {
    "/v2/options/contracts".into()
  }

  fn query(input: &Self::Input) -> Result<Option<Str>, Self::ConversionError> {
    Ok(Some(to_query(input)?.into()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use serde_json::from_slice as from_json;
  use serde_json::to_vec as to_json;

  use test_log::test;

  use crate::api_info::ApiInfo;
  use crate::Client;

  /// Check that we can serialize and deserialize a [`ListReq`].
  #[test]
  fn serialize_deserialize_list_request() {
    let request = ListReq {
      underlying_symbols: Some("AAPL,TSLA".into()),
      status: Status::Active,
      ..Default::default()
    };

    let json = to_json(&request).unwrap();
    assert_eq!(from_json::<ListReq>(&json).unwrap(), request);
  }

  /// Make sure that we can list available options contracts.
  #[test(tokio::test)]
  async fn list_options_contracts() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let request = ListReq::default();
    let res = client.issue::<List>(&request).await.unwrap();

    let contract = res
      .option_contracts
      .iter()
      .find(|x| x.underlying_symbol == "AA");
    assert!(contract.is_some());
  }

  /// Make sure that we can filter available options contracts by underlying asset.
  #[test(tokio::test)]
  async fn list_filtered_options_contracts() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let request = ListReqInit {
      underlying_symbols: Some(vec!["AAPL"]),
      ..Default::default()
    }
    .init(None);
    let res = client.issue::<List>(&request).await.unwrap();

    let contract = res
      .option_contracts
      .iter()
      .find(|x| x.underlying_symbol == "AAPL");
    assert!(contract.is_some());
  }

  /// Verify that we can request data through a provided page token.
  #[test(tokio::test)]
  async fn can_follow_pagination() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let mut request = ListReqInit::<String> {
      limit: Some(2),
      ..Default::default()
    }
    .init(None);
    let mut res = client.issue::<List>(&request).await.unwrap();
    let contracts = res.option_contracts;

    assert_eq!(contracts.len(), 2);
    request.page_token = res.next_page_token;

    res = client.issue::<List>(&request).await.unwrap();
    let new_contracts = res.option_contracts;

    assert_eq!(new_contracts.len(), 2);
    assert_ne!(contracts[0].symbol, new_contracts[0].symbol);
    assert_ne!(contracts[0].symbol, new_contracts[1].symbol);
    assert_ne!(contracts[1].symbol, new_contracts[0].symbol);
    assert_ne!(contracts[1].symbol, new_contracts[1].symbol);
    assert!(res.next_page_token.is_some())
  }
}
