import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/option.{type Option, None, Some}

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

@external(erlang, "gleamail_ffi", "smtpopts_to_gensmtp_options")
fn smtpopts_to_gensmtp_options(opts: SmtpOpts) -> List(Dynamic)

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

/// also sets the port to 465
pub fn with_ssl(opts) {
  SmtpOpts(..opts, ssl: True, port: 465)
}

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

pub fn main() -> Nil {
  smtpopts_to_gensmtp_options(
    new() |> with_relay(Hostname("smtp.gmail.com")) |> with_port(456),
  )
  io.println("Hello from gleemail!")
}
