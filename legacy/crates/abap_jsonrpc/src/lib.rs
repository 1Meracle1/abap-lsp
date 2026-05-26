use std::io::{self, BufRead, Write};

use serde::{Deserialize, Serialize};

pub const JSON_RPC_VERSION: &str = "2.0";

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: serde_json::Value,
    pub method: String,
    #[serde(default)]
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Notification {
    pub jsonrpc: String,
    pub method: String,
    #[serde(default)]
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Response {
    pub jsonrpc: String,
    pub id: serde_json::Value,
    #[serde(default)]
    pub result: Option<serde_json::Value>,
    #[serde(default)]
    pub error: Option<ResponseError>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResponseError {
    pub code: i64,
    pub message: String,
}

#[derive(Debug, thiserror::Error)]
pub enum FrameError {
    #[error("missing Content-Length header")]
    MissingContentLength,
    #[error("invalid Content-Length header value: {0}")]
    InvalidContentLength(String),
    #[error("io error: {0}")]
    Io(#[from] io::Error),
}

pub fn read_frame<R>(reader: &mut R) -> Result<Option<Vec<u8>>, FrameError>
where
    R: BufRead,
{
    let mut content_length = None;
    let mut line = String::new();

    loop {
        line.clear();
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 {
            if content_length.is_none() {
                return Ok(None);
            }
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated header").into());
        }

        if line == "\r\n" || line == "\n" {
            break;
        }

        if let Some((name, value)) = line.split_once(':') {
            if name.trim().eq_ignore_ascii_case("Content-Length") {
                let raw = value.trim();
                let parsed = raw
                    .parse::<usize>()
                    .map_err(|_| FrameError::InvalidContentLength(raw.to_owned()))?;
                content_length = Some(parsed);
            }
        }
    }

    let content_length = content_length.ok_or(FrameError::MissingContentLength)?;
    let mut payload = vec![0_u8; content_length];
    reader.read_exact(&mut payload)?;
    Ok(Some(payload))
}

pub fn write_frame<W>(writer: &mut W, payload: &[u8]) -> Result<(), FrameError>
where
    W: Write,
{
    write!(writer, "Content-Length: {}\r\n\r\n", payload.len())?;
    writer.write_all(payload)?;
    writer.flush()?;
    Ok(())
}

impl Response {
    pub fn success(id: serde_json::Value, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: JSON_RPC_VERSION.to_owned(),
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn failure(id: serde_json::Value, code: i64, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: JSON_RPC_VERSION.to_owned(),
            id,
            result: None,
            error: Some(ResponseError {
                code,
                message: message.into(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::{read_frame, write_frame};

    #[test]
    fn round_trips_frame_payload() {
        let payload = br#"{"jsonrpc":"2.0","method":"initialize"}"#;
        let mut encoded = Vec::new();
        write_frame(&mut encoded, payload).expect("frame should encode");

        let mut reader = Cursor::new(encoded);
        let decoded = read_frame(&mut reader)
            .expect("frame should decode")
            .expect("payload should be present");

        assert_eq!(decoded, payload);
    }
}
