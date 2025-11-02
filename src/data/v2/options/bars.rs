use crate::data::v2::bars::{Bar, TimeFrame};
use crate::data::DATA_BASE_URL;
use crate::Str;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_urlencoded::to_string as to_query;
use std::collections::HashMap;

/// A GET request to be issued to the /v1beta1/options/bars endpoint.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ListReq {
  /// The option contract symbol for which to retrieve market data.
  pub symbols: String,
  /// The maximum number of bars to be returned for each symbol.
  ///
  /// It can be between 1 and 10000. Defaults to 1000 if the provided
  /// value is None.
  #[serde(rename = "limit")]
  pub limit: Option<usize>,
  /// Filter bars equal to or after this time.
  #[serde(rename = "start")]
  pub start: DateTime<Utc>,
  /// Filter bars equal to or before this time.
  #[serde(rename = "end")]
  pub end: DateTime<Utc>,
  /// The time frame for the bars.
  #[serde(rename = "timeframe")]
  pub timeframe: TimeFrame,
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
pub struct ListReqInit {
  /// See `ListReq::limit`.
  pub limit: Option<usize>,
  /// See `ListReq::page_token`.
  pub page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  pub _non_exhaustive: (),
}

impl ListReqInit {
  /// Create a [`ListReq`] from a `ListReqInit`.
  #[inline]
  pub fn init<S>(
    self,
    symbols: Vec<S>,
    start: DateTime<Utc>,
    end: DateTime<Utc>,
    timeframe: TimeFrame,
  ) -> ListReq
  where
    S: Into<String>,
  {
    let symbols = symbols
      .into_iter()
      .map(Into::into)
      .collect::<Vec<String>>()
      .join(",");
    ListReq {
      symbols,
      start,
      end,
      timeframe,
      limit: self.limit,
      page_token: self.page_token,
      _non_exhaustive: (),
    }
  }
}

/// A collection of bars as returned by the API. This is one page of bars.
#[derive(Debug, Deserialize, Eq, PartialEq)]
pub struct Bars {
  /// The list of returned bars.
  #[serde(rename = "bars")]
  pub bars: HashMap<Str, Vec<Bar>>,
  /// The token to provide to a request to get the next page of bars for
  /// this request.
  #[serde(rename = "next_page_token")]
  pub next_page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}

Endpoint! {
  /// The representation of a GET request to the /v1beta1/options/bars endpoint.
  pub List(ListReq),
  Ok => Bars, [
    /// The market data was retrieved successfully.
    /* 200 */ OK,
  ],
  Err => ListError, [
    /// A query parameter was invalid.
    /* 400 */ BAD_REQUEST => InvalidInput,
  ]

  fn base_url() -> Option<Str> {
    Some(DATA_BASE_URL.into())
  }

  fn path(_input: &Self::Input) -> Str {
    "/v1beta1/options/bars".into()
  }

  fn query(input: &Self::Input) -> Result<Option<Str>, Self::ConversionError> {
    Ok(Some(to_query(input)?.into()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::str::FromStr as _;

  use http_endpoint::Endpoint;
  use num_decimal::Num;
  use serde_json::from_str as from_json;

  use test_log::test;

  use crate::api_info::ApiInfo;
  use crate::Client;
  use crate::RequestError;

  /// Verify that we can properly parse a reference bar response.
  #[test]
  fn parse_reference_bars() {
    let response = r#"{
  "bars": {
    "AAPL240315C00225000": [
      {
        "c": 0.1,
        "h": 0.1,
        "l": 0.1,
        "n": 1,
        "o": 0.1,
        "t": "2024-01-18T14:36:00Z",
        "v": 5,
        "vw": 0.1
      },
      {
        "c": 0.11,
        "h": 0.11,
        "l": 0.11,
        "n": 6,
        "o": 0.11,
        "t": "2024-01-18T14:42:00Z",
        "v": 50,
        "vw": 0.11
      }
    ]
  },
  "next_page_token": "QUFQTDI0MDMxNUMwMDIyNTAwMHxNfDE3MDU1ODg5ODAwMDAwMDAwMDA="
}"#;

    let res = from_json::<<List as Endpoint>::Output>(response).unwrap();
    let bars_map = res.bars;
    let expected_symbol = "AAPL240315C00225000";
    assert!(bars_map.contains_key(expected_symbol));
    let bars = bars_map.get(expected_symbol).unwrap();
    let expected_time = DateTime::<Utc>::from_str("2024-01-18T14:36:00Z").unwrap();
    assert_eq!(bars.len(), 2);
    assert_eq!(bars[0].time, expected_time);
    assert_eq!(bars[0].open, Num::new(10, 100));
    assert_eq!(bars[0].close, Num::new(10, 100));
    assert_eq!(bars[0].high, Num::new(10, 100));
    assert_eq!(bars[0].low, Num::new(10, 100));
    assert_eq!(bars[0].weighted_average, Num::new(10, 100));
    assert_eq!(bars[0].volume, 5);
    assert!(res.next_page_token.is_some())
  }

  /// Check that we can decode a response containing no bars correctly.
  #[test(tokio::test)]
  async fn no_bars() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let start = DateTime::from_str("2025-01-10T00:00:00Z").unwrap();
    let end = DateTime::from_str("2025-01-11T00:00:00Z").unwrap();
    let request =
      ListReqInit::default().init(vec!["AAPL241220C00300000"], start, end, TimeFrame::OneDay);

    let res = client.issue::<List>(&request).await.unwrap();
    assert_eq!(res.bars, HashMap::new());
  }

  /// Check that we can request historic bar data for an option contract.
  #[test(tokio::test)]
  async fn request_bars() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let start = DateTime::from_str("2024-01-03T00:00:00Z").unwrap();
    let end = DateTime::from_str("2024-01-19T00:00:00Z").unwrap();
    let request = ListReqInit {
      ..Default::default()
    }
    .init(
      vec!["AAPL241220C00300000", "AAPL240315C00225000"],
      start,
      end,
      TimeFrame::OneMinute,
    );

    let res = client.issue::<List>(&request).await.unwrap();
    let bars_map = res.bars;

    assert_eq!(bars_map.len(), 2);
    assert!(bars_map.contains_key("AAPL241220C00300000"));
    assert!(bars_map.contains_key("AAPL240315C00225000"));

    let strike_300 = bars_map.get("AAPL241220C00300000").unwrap();
    assert_eq!(
      strike_300[0].time,
      DateTime::<Utc>::from_str("2024-01-18T15:22:00Z").unwrap()
    );
    assert_eq!(strike_300[0].open, Num::new(24, 100));
    assert_eq!(strike_300[0].close, Num::new(24, 100));
    assert_eq!(strike_300[0].high, Num::new(24, 100));
    assert_eq!(strike_300[0].low, Num::new(24, 100));
    assert_eq!(strike_300[0].weighted_average, Num::new(24, 100));
    assert_eq!(strike_300[0].volume, 9);

    let strike_225 = bars_map.get("AAPL240315C00225000").unwrap();
    assert_eq!(
      strike_225[0].time,
      DateTime::<Utc>::from_str("2024-01-18T14:36:00Z").unwrap()
    );
    assert_eq!(strike_225[0].open, Num::new(10, 100));
    assert_eq!(strike_225[0].close, Num::new(10, 100));
    assert_eq!(strike_225[0].high, Num::new(10, 100));
    assert_eq!(strike_225[0].low, Num::new(10, 100));
    assert_eq!(strike_225[0].weighted_average, Num::new(10, 100));
    assert_eq!(strike_225[0].volume, 5);
  }

  /// Verify that we can request data through a provided page token.
  #[test(tokio::test)]
  async fn can_follow_pagination() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let start = DateTime::from_str("2024-01-03T00:00:00Z").unwrap();
    let end = DateTime::from_str("2024-01-20T00:00:00Z").unwrap();
    let mut request = ListReqInit {
      limit: Some(2),
      ..Default::default()
    }
    .init(
      vec!["AAPL240315C00225000"],
      start,
      end,
      TimeFrame::OneMinute,
    );

    let mut res = client.issue::<List>(&request).await.unwrap();
    let bars_map = res.bars;

    assert_eq!(bars_map.len(), 1);
    assert!(bars_map.contains_key("AAPL240315C00225000"));

    let bars = bars_map.get("AAPL240315C00225000").unwrap();
    assert_eq!(bars.len(), 2);

    request.page_token = res.next_page_token;

    res = client.issue::<List>(&request).await.unwrap();
    let new_bars_map = res.bars;

    assert_eq!(new_bars_map.len(), 1);
    assert!(new_bars_map.contains_key("AAPL240315C00225000"));

    let new_bars = new_bars_map.get("AAPL240315C00225000").unwrap();
    assert_eq!(new_bars.len(), 2);
    assert!(new_bars[0].time > bars[1].time);
    assert!(res.next_page_token.is_some())
  }

  /// Check that we fail as expected when an invalid page token is
  /// specified.
  #[test(tokio::test)]
  async fn invalid_page_token() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);

    let start = DateTime::from_str("2024-01-03T00:00:00Z").unwrap();
    let end = DateTime::from_str("2024-01-20T00:00:00Z").unwrap();
    let request = ListReqInit {
      page_token: Some("123456789abcdefghi".to_string()),
      ..Default::default()
    }
    .init(
      vec!["AAPL240315C00225000"],
      start,
      end,
      TimeFrame::OneMinute,
    );

    let err = client.issue::<List>(&request).await.unwrap_err();
    match err {
      RequestError::Endpoint(ListError::InvalidInput(_)) => (),
      _ => panic!("Received unexpected error: {err:?}"),
    };
  }

  /// Verify that we get no entries in the bars hash map when we provide an invalid symbol.
  #[test(tokio::test)]
  async fn invalid_symbol() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);

    let start = DateTime::from_str("2024-01-03T00:00:00Z").unwrap();
    let end = DateTime::from_str("2024-01-20T00:00:00Z").unwrap();
    let request = ListReqInit::default().init(vec!["ABC123"], start, end, TimeFrame::OneDay);

    let err = client.issue::<List>(&request).await.unwrap_err();
    match err {
      RequestError::Endpoint(ListError::InvalidInput(Ok(_))) => (),
      _ => panic!("Received unexpected error: {err:?}"),
    };
  }
}
