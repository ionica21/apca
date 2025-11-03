use chrono::{DateTime, TimeZone, Utc};
use rmpv::Value;
use serde::{de, Deserialize, Deserializer};

/// Deserialize a timestamp that may be:
/// - RFC3339 string
/// - integer epoch (s/ms/us/ns)
/// - MsgPack timestamp extension (-1) presented as a newtype struct
pub fn dt_any<'de, D>(de: D) -> Result<DateTime<Utc>, D::Error>
where
  D: Deserializer<'de>,
{
  struct V;
  impl<'de> de::Visitor<'de> for V {
    type Value = DateTime<Utc>;

    fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_str("RFC3339 string or MsgPack timestamp ext (-1)")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
      E: de::Error,
    {
      DateTime::parse_from_rfc3339(s)
        .map(|dt| dt.with_timezone(&Utc))
        .map_err(E::custom)
    }

    /// MsgPack timestamp ext (-1) comes through Serde as a newtype struct.
    /// We deserialize into `rmpv::Value` and manually decode the ext payload.
    fn visit_newtype_struct<D2>(self, d2: D2) -> Result<Self::Value, D2::Error>
    where
      D2: Deserializer<'de>,
    {
      let v = Value::deserialize(d2)?;
      if let Some(dt) = try_decode_msgpack_ts_value::<D2::Error>(&v)? {
        return Ok(dt);
      }
      Err(de::Error::custom(format!("expected MsgPack timestamp ext (-1), got {v:?}")))
    }
  }

  de.deserialize_any(V)
}

/// Accept both canonical `Ext(-1, bytes)` and array form `[-1, <binary>]`.
fn try_decode_msgpack_ts_value<E>(v: &Value) -> Result<Option<DateTime<Utc>>, E>
where
  E: de::Error,
{
  match v {
    Value::Ext(ext_type, data) if *ext_type == -1 => parse_msgpack_ts::<E>(data).map(Some),
    Value::Array(items) if items.len() == 2 => {
      let ext_type = match &items[0] {
        Value::Integer(i) => i
          .as_i64()
          .ok_or_else(|| E::custom("non-integer ext type"))?,
        _ => return Ok(None),
      };
      if ext_type != -1 {
        return Ok(None);
      }
      let data = match &items[1] {
        Value::Binary(b) => &b[..],
        _ => return Err(E::custom("ext payload not binary")),
      };
      parse_msgpack_ts::<E>(data).map(Some)
    },
    _ => Ok(None),
  }
}

/// Decode MsgPack timestamp ext payload (per spec).
fn parse_msgpack_ts<E>(data: &[u8]) -> Result<DateTime<Utc>, E>
where
  E: de::Error,
{
  match data.len() {
    // 32-bit seconds (u32)
    4 => {
      let secs = u32::from_be_bytes([data[0], data[1], data[2], data[3]]) as i64;
      Utc
        .timestamp_opt(secs, 0)
        .single()
        .ok_or_else(|| E::custom("invalid 32-bit ext ts"))
    },
    // 64-bit: 30-bit nanos (high) + 34-bit seconds (low, unsigned)
    8 => {
      let u = u64::from_be_bytes([
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
      ]);
      let nanos = (u >> 34) as u32; // top 30 bits
      let secs = (u & 0x3FFF_FFFF) as i64; // low 34 bits
      Utc
        .timestamp_opt(secs, nanos)
        .single()
        .ok_or_else(|| E::custom("invalid 64-bit ext ts"))
    },
    // 96-bit: 32-bit nanos (u32) + 64-bit seconds (i64)
    12 => {
      let nanos = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
      let secs = i64::from_be_bytes([
        data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11],
      ]);
      Utc
        .timestamp_opt(secs, nanos)
        .single()
        .ok_or_else(|| E::custom("invalid 96-bit ext ts"))
    },
    _ => Err(E::custom(format!(
      "unexpected MsgPack timestamp ext payload length {}",
      data.len()
    ))),
  }
}
