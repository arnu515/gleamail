import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/string_tree

/// These are headers that are sent along with the mail.
///
/// Their format (and a list of common headers) is described in
/// [RFC 5322](https://datatracker.ietf.org/doc/html/rfc5322).
///
/// [Whitespace folding](https://datatracker.ietf.org/doc/html/rfc5322#section-2.2.3)
/// is not handled automatically. You may use the [fold](#fold) function
/// to automatically fold the header's value.
///
/// Some headers, like Subject, To, From, CC, BCC, Content-Type, etc.
/// are automatically set by this library. To override them, you can
/// add the `Set` or `Multiple` variants, or to remove them, you can
/// add the `Remove` variant to the list of extra headers in the mail.
///
/// Specifiying variants with the same header key multiple times will
/// only result in the one appearing last to be applied.
///
pub type MailHeader {
  /// Use this to not send a default header.
  ///
  /// For example, to not send any "CC: " headers, add
  /// `Remove("CC")` to the header
  Remove(key: String)
  /// Sets a header to a singular value
  Set(key: String, value: String)
  /// Sets a header to multiple values. This header will be
  /// repeated multiple times in the mail body with each
  /// of the values
  Multiple(key: String, values: List(String))
}

/// Convert a list of `MailHeader`s to a list of key-value pairs
pub fn mail_headers_to_key_value_pairs(
  headers in: List(MailHeader),
) -> List(#(String, String)) {
  populate_mail_headers_dict(dict.new(), in)
  |> dict.to_list()
  |> list.flat_map(fn(x) {
    let #(k, v) = x
    case v {
      [] -> [#(k, "")]
      [v] -> [#(k, v)]
      v -> list.map(v, fn(v) { #(k, v) })
    }
  })
}

fn populate_mail_headers_dict(
  dict: dict.Dict(String, List(String)),
  headers: List(MailHeader),
) -> dict.Dict(String, List(String)) {
  case headers {
    [] -> dict
    [header, ..rest] ->
      populate_mail_headers_dict(
        case header {
          Set(key:, value:) -> dict.insert(dict, key, [value])
          Multiple(key:, values:) ->
            dict.upsert(dict, key, fn(x) {
              case x {
                option.None -> values
                option.Some(old) -> list.append(old, values)
              }
            })
          Remove(key:) -> dict.delete(dict, key)
        },
        rest,
      )
  }
}

pub type AttachmentEncoding {
  Base64
  QuotedPrintable
  Ascii
}

pub type MailAttachment {
  MailAttachment(
    content: BitArray,
    filename: String,
    mime_type: #(String, String),
    encoding: AttachmentEncoding,
  )
}

/// Create a Mail using [`make`](#make)
pub type Mail {
  /// Do NOT construct a Mail by yourself, but use the convenience
  /// `with_*` methods instead, since they have validation checks.
  Mail(
    subject: String,
    from: Address,
    to: List(Address),
    cc: List(Address),
    bcc: List(Address),
    body: List(Body),
    attachments: List(MailAttachment),
    extra_headers: List(MailHeader),
  )
}

pub type Address {
  Address(name: Option(String), email: String)
}

/// Converts an email address to a string representation, i.e.
/// `name <email>`.
pub fn address_to_string(addr: Address) -> String {
  let Address(name:, email:) = addr
  case name {
    option.Some(name) -> name <> " <" <> email <> ">"
    _ -> email
  }
}

pub type Body {
  PlainText(String)
  Html(String)
}

/// Create a new `Mail`
///
/// `to` must have at least one address.
///
pub fn make(
  subject subject: String,
  from from: Address,
  to to: List(Address),
) -> Mail {
  let assert [_addr, ..] = to
  Mail(subject:, from:, to:, cc: [], bcc: [], body: [], extra_headers: [])
}

/// Add an `address` to the `mail`'s Cc list
///
/// This address will appear at the top of the current Cc list.
///
pub fn with_cc(mail: Mail, addr: Address) -> Mail {
  Mail(..mail, cc: [addr, ..mail.cc])
}

/// Add an `address` to the `mail`'s Bcc list
///
/// This address will appear at the top of the current Bcc list.
///
pub fn with_bcc(mail: Mail, addr: Address) -> Mail {
  Mail(..mail, bcc: [addr, ..mail.bcc])
}

/// Set the `mail`'s Cc `list`
pub fn with_cc_list(mail: Mail, list: List(Address)) -> Mail {
  Mail(..mail, cc: list)
}

/// Set the `mail`'s Bcc `list`
pub fn with_bcc_list(mail: Mail, list: List(Address)) -> Mail {
  Mail(..mail, bcc: list)
}

/// Add a `body` to a `mail`
pub fn with_body(mail: Mail, body: Body) -> Mail {
  Mail(..mail, body: [body, ..mail.body])
}

/// Add a plain text `body` to a `mail`
pub fn with_plaintext_body(mail: Mail, text: String) -> Mail {
  with_body(mail, PlainText(text))
}

/// Add an HTML `body` to a `mail`
pub fn with_html_body(mail: Mail, html: String) -> Mail {
  with_body(mail, Html(html))
}

/// Add a `header` to a `mail`
pub fn with_header(mail: Mail, header: MailHeader) -> Mail {
  Mail(..mail, extra_headers: [header, ..mail.extra_headers])
}

/// The maximum permitted line length of a header (including the key,
/// colon separator, and whitespace between the key and value, but
/// excluding the ending `\r\n`).
pub const max_length = 998

/// The recommended line length of a header (same inclusion and exclusion
/// conditions as [max_length](#max-length)).
pub const recommended_length = 78

/// Folds the header's `value` according to
/// [RFC 5322, 3.2.2](https://datatracker.ietf.org/doc/html/rfc5322/#section-3.2.2)
/// to a max size of `max_length`, where the length of the header key is `key_length`.
///
/// Set the `key_length` to `-2` if there is no key.
///
/// The value should ideally not have any line breaks.
///
/// `max_length` does NOT include the ending `\r\n` bytes. `key_length` should only
/// be the length of the key, not including the `: `, which is handled automatically.
///
/// For example:
///
/// ```gleam
/// Set(key: "something", value: mail.fold("loooong value", mail.MAX_LENGTH, string.byte_size("something")))
/// ```
///
/// The two provided lengths are [max_length](#max-length), which is the maximum
/// permitted length, of 998 chars; and [recommended_length](#recommended-length),
/// which is a recommendation by the spec, of 78 chars.
/// 
pub fn fold(
  value: String,
  max_length max_length: Int,
  key_length key_length: Int,
) -> String {
  let first_line_length = max_length - { key_length + 2 }
  let first_line = string.slice(value, 0, first_line_length)
  let value = string.drop_start(value, first_line_length)
  case value {
    "" -> first_line
    value -> {
      string.append(first_line, "\r\n")
      |> string_tree.from_string()
      |> do_fold(
        // -1 for the space at the start
        max_length - 1,
        value,
      )
    }
  }
}

fn do_fold(tree: string_tree.StringTree, len: Int, value: String) -> String {
  case value {
    "" -> string_tree.to_string(tree)
    value -> {
      let tree = string_tree.append(tree, " ")
      let tree =
        string.slice(value, 0, len)
        |> string_tree.append(tree, _)
      let value = string.drop_start(value, len)
      case value {
        "" -> string_tree.to_string(tree)
        value -> {
          string_tree.append(tree, "\r\n")
          |> do_fold(len, value)
        }
      }
    }
  }
}
