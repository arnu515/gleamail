import gleam/option.{type Option, None, Some}
import gleamail/error
import gleamail/internal/gen_smtp_client
import gleamail/mail

// TODO: rename this type to something better
pub type UseOpt {
  Always
  Never
  IfAvailable
}

pub type RelayOpt {
  /// a hostname
  Hostname(String)
  /// four octets (8-bit uint)
  Ipv4Address(Int, Int, Int, Int)
  /// eight hextets (16-bit uint)
  Ipv6Address(Int, Int, Int, Int, Int, Int, Int, Int)
}

pub type SmtpOpts {
  SmtpOpts(
    ssl: Bool,
    tls: UseOpt,
    // TODO: add tls_options
    // TODO: add sockopts
    /// Allowed range: 0..=65535
    port: Int,
    timeout: Option(Int),
    relay: Option(RelayOpt),
    no_mx_lookups: Bool,
    auth: UseOpt,
    hostname: Option(String),
    retries: Int,
    username: Option(String),
    password: Option(String),
    /// if true: quits on transaction error.  
    /// if false: resets on transaction error.  
    quit_on_transaction_error: Option(Bool),
  )
}

pub fn new() {
  SmtpOpts(
    ssl: False,
    tls: IfAvailable,
    port: 25,
    timeout: None,
    relay: None,
    no_mx_lookups: False,
    auth: IfAvailable,
    hostname: None,
    retries: 1,
    username: None,
    password: None,
    quit_on_transaction_error: None,
  )
}

/// Connect with SSL. Sets the port to 465
pub fn with_ssl(opts) {
  SmtpOpts(..opts, ssl: True, port: 465)
}

/// Use STARTTLS
pub fn with_tls(opts, tls: UseOpt) {
  SmtpOpts(..opts, tls:)
}

pub fn with_port(opts, port: Int) {
  SmtpOpts(..opts, port:)
}

pub fn with_timeout(opts, timeout: Option(Int)) {
  SmtpOpts(..opts, timeout:)
}

pub fn with_relay(opts, relay: RelayOpt) {
  SmtpOpts(..opts, relay: Some(relay))
}

pub fn with_no_relay(opts) {
  SmtpOpts(..opts, relay: None)
}

pub fn with_no_mx_lookups(opts) {
  SmtpOpts(..opts, no_mx_lookups: True)
}

pub fn with_auth(opts, auth: UseOpt) {
  SmtpOpts(..opts, auth:)
}

pub fn with_hostname(opts, hostname: Option(String)) {
  SmtpOpts(..opts, hostname:)
}

pub fn with_retries(opts, retries: Int) {
  SmtpOpts(..opts, retries:)
}

pub fn with_username(opts, username: Option(String)) {
  SmtpOpts(..opts, username:)
}

pub fn with_password(opts, password: Option(String)) {
  SmtpOpts(..opts, password:)
}

pub fn with_quit_on_transaction_error(
  opts,
  quit_on_transaction_error: Option(Bool),
) {
  SmtpOpts(..opts, quit_on_transaction_error:)
}

pub type SmtpSocket =
  gen_smtp_client.GenSmtpSocket

/// Open an SMTP client socket
pub fn open(opts: SmtpOpts) -> Result(SmtpSocket, error.OpenError) {
  smtpopts_to_gensmtp_options(opts)
  |> gen_smtp_client.open
}

/// Closes an open SMTP client socket
pub fn close(sock: SmtpSocket) -> Nil {
  gen_smtp_client.close(sock)
}

/// Opens an SMTP client socket and calls the `callback` with
/// it. Closes the socket when the callback is done. This function
/// is made to be used with the `use` syntax to ensure that the
/// mail socket is closed.
///
/// ## Example
///
/// ```gleam
/// import gleam/option.{Some, None}
/// import gleamail/client.{Address}
/// import gleamail/mail.{Address, with_plaintext_body}
///
/// pub fn main() -> Nil {
///   let opts = client.new()
///     |> client.with_relay("smtp.example.org")
///     |> client.with_username("something")
///     |> client.with_password("*********")
///   use sock <- client.smtp(opts)
///   client.deliver(mail.make(
///     subject: "Hello, world!"
///     from: Address(name: None, email: "me@example.org"),
///     to: [Address(name: Some("You"), email: "you@example.com")]
///   ) |> with_plaintext_body("Hello, world!"))
/// }
/// ```
///
pub fn smtp(
  opts: SmtpOpts,
  cb callback: fn(Result(SmtpSocket, error.OpenError)) -> any,
) -> Nil {
  let sock = open(opts)
  callback(sock)
  case sock {
    Ok(sock) -> close(sock)
    _ -> Nil
  }
}

/// Send an email. This function blocks. The socket may be
/// reused only if this function returned Ok, or if it returned
/// error and the option quit_on_transaction_error is Some(False).
///
/// Returns the SMTP server's receipt identifier on success.
///
pub fn deliver_blocking(
  sock: SmtpSocket,
  mail mail: mail.Mail,
) -> Result(BitArray, error.Failure) {
  gen_smtp_client.deliver(sock, gen_smtp_client.encode(mail))
}

@external(erlang, "gleamail_ffi", "smtpopts_to_gensmtp_options")
fn smtpopts_to_gensmtp_options(opts: SmtpOpts) -> gen_smtp_client.GenSmtpOptions
