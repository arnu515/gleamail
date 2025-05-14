import gleam/list
import gleam/string
import gleamail/error.{type Failure, type OpenError}
import gleamail/mail

pub type GenSmtpOptions

pub type GenSmtpSocket

/// {From, To, Body}
/// https://hexdocs.pm/gen_smtp/gen_smtp_client.html#t:email/0
pub type GenSmtpEmail =
  #(String, List(String), BitArray)

@external(erlang, "gen_smtp_client", "open")
pub fn open(opts: GenSmtpOptions) -> Result(GenSmtpSocket, OpenError)

@external(erlang, "gen_smtp_client", "close")
pub fn close(sock: GenSmtpSocket) -> Nil

//     ^^^^^ actually returns `ok`.

@external(erlang, "gen_smtp_client", "deliver")
pub fn deliver(
  sock: GenSmtpSocket,
  email: GenSmtpEmail,
) -> Result(BitArray, Failure)

type Mimetuple

@external(erlang, "gleamail_ffi", "get_mimetuple")
fn get_mimetuple(
  typ: String,
  subtype: String,
  headers: List(#(String, String)),
  body: List(String),
) -> Mimetuple

@external(erlang, "mimemail", "encode")
fn encode_mimetuple(mt: Mimetuple) -> BitArray

/// Convert mail.Mail to an email that is understood by gen_smtp
pub fn encode(mail: mail.Mail) -> GenSmtpEmail {
  let mail.Mail(
    from:,
    to:,
    cc:,
    bcc:,
    subject:,
    body: mail_body,
    extra_headers:,
  ) = mail
  let assert [_, ..] = to as "'To' must have at least one email"

  let set_header = fn(key: String, value: String) {
    mail.Set(
      key:,
      value: mail.fold(value, mail.max_length, string.byte_size(key)),
    )
  }

  let headers: List(mail.MailHeader) = [
    set_header("From", mail.address_to_string(from)),
    set_header("To", string.join(list.map(to, mail.address_to_string), ", ")),
    set_header("Subject", subject),
  ]
  let headers = case cc {
    [] -> headers
    cc -> [
      set_header("Cc", string.join(list.map(cc, mail.address_to_string), ", ")),
      ..headers
    ]
  }
  let headers = case bcc {
    [] -> headers
    bcc -> [
      set_header(
        "Bcc",
        string.join(list.map(bcc, mail.address_to_string), ", "),
      ),
      ..headers
    ]
  }
  let headers = list.append(headers, extra_headers)
  let headers = mail.mail_headers_to_key_value_pairs(headers)

  // Reversing the list because new bodies get prepended at the start
  let body = get_email_body(list.reverse(mail_body))

  let #(mime_type, mime_subtype, body) = case body {
    TextPlain(body) -> #("text", "plain", [mail.fold(body, mail.max_length, -2)])
    TextHtml(body) -> #("text", "html", [mail.fold(body, mail.max_length, -2)])
    Alternative(plain:, html:) -> #("multipart", "alternative", [
      mail.fold(plain, mail.max_length, -2),
      mail.fold(html, mail.max_length, -2),
    ])
  }

  #(
    from.email,
    list.map(list.flatten([to, cc, bcc]), fn(x: mail.Address) { x.email }),
    encode_mimetuple(get_mimetuple(mime_type, mime_subtype, headers, body)),
  )
}

type EmailBody {
  TextPlain(String)
  TextHtml(String)
  Alternative(plain: String, html: String)
}

fn get_email_body(bodies: List(mail.Body)) -> EmailBody {
  case bodies {
    [] -> TextPlain("")
    [body, ..bodies] -> {
      let curr_body = case body {
        mail.PlainText(text) -> TextPlain(text)
        mail.Html(html) -> TextHtml(html)
      }
      do_get_email_body(bodies, curr_body)
    }
  }
}

fn do_get_email_body(bodies: List(mail.Body), curr_body: EmailBody) -> EmailBody {
  case bodies {
    [] -> curr_body
    [body, ..bodies] ->
      do_get_email_body(bodies, norm_email_body(body, curr_body))
  }
}

fn norm_email_body(body: mail.Body, curr_body: EmailBody) -> EmailBody {
  case curr_body, body {
    TextPlain(..), mail.PlainText(text) -> TextPlain(text)
    TextPlain(plain), mail.Html(html) -> Alternative(plain:, html:)
    TextHtml(..), mail.Html(html) -> TextHtml(html)
    TextHtml(html), mail.PlainText(plain) -> Alternative(plain:, html:)
    Alternative(html:, ..), mail.PlainText(plain) -> Alternative(plain:, html:)
    Alternative(plain:, ..), mail.Html(html) -> Alternative(plain:, html:)
  }
}
