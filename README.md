# gleamail

[![Package Version](https://img.shields.io/hexpm/v/gleamail)](https://hex.pm/packages/gleamail)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleamail/)

Gleam SMTP client! (built on top of [`gen_smtp`](https://hex.pm/packages/gen_smtp))

It is a type-safe and friendly wrapper on top of the excellent [`gen_smtp`](https://hexdocs.pm/gen_smtp) Erlang library.
Currently, it supports sending mails via. blocking the current process, but I will add support for non-blocking delivery soon.

```sh
gleam add gleamail@1
```

```gleam
import gleamail/client.{Never}
import gleamail/mail

pub fn main() -> Nil {
  // Start a https://mailpit.axllent.org server on smtp
  // port 1025 locally for testing
  let smtp_host = "localhost"
  let smtp_port = 1025

  // Configure client options: Set the relay, port,
  // and disable auth and tls
  let opts =
    client.new()
    |> client.with_relay(client.Hostname(smtp_host))
    |> client.with_port(smtp_port)
    |> client.with_auth(Never)
    |> client.with_tls(Never)

  // Open the connection
  let assert Ok(sock) = client.open(opts)

  echo sock

  // Deliver a simple mail with no body
  // This function blocks the current process
  let assert Ok(rcpt) =
    client.deliver_blocking(
      sock,
      mail.make(
        "Hello, world!",
        mail.Address(name: Some("Me"), email: "me@example.com"),
        to: [mail.Address(name: Some("you"), email: "you@example.com")],
      ),
    )

  // The raw receipt response set by the SMTP server
  // Usually looks something like:
  // "2.0.0 Ok: queued as 4uDb3MzBaJYfWDD8cLXqvs\r\n"
  echo rcpt

  // Deliver a mail with a plain text body
  let assert Ok(rcpt) =
    client.deliver_blocking(
      sock,
      mail.make(
        "Hello, world!",
        mail.Address(name: Some("Me"), email: "me@example.com"),
        to: [mail.Address(name: Some("you"), email: "you@example.com")],
      )
        |> mail.with_plaintext_body("Hello, world!"),
    )

  echo rcpt

  // Deliver a mail with an HTML body
  let assert Ok(rcpt) =
    client.deliver_blocking(
      sock,
      mail.make(
        "Hello, world!",
        mail.Address(name: Some("Me"), email: "me@example.com"),
        to: [mail.Address(name: Some("you"), email: "you@example.com")],
      )
        |> mail.with_html_body("<h1>Hello, world!</h1>"),
    )

  echo rcpt

  // Combine and override bodies!
  // If both plaintext and html bodies are specified, the
  // mail is sent as mimepart/alternative
  let assert Ok(rcpt) =
    client.deliver_blocking(
      sock,
      mail.make(
        "Hello, world!",
        mail.Address(name: Some("Me"), email: "me@example.com"),
        to: [mail.Address(name: Some("you"), email: "you@example.com")],
      )
        |> mail.with_plaintext_body("Hello, world!")
        |> mail.with_html_body("<h1>Hello, world!</h1>")
        // this overrides the previous plaintext body
        |> mail.with_plaintext_body("Namaste, world!")
        // this overrides the previous html body
        |> mail.with_html_body("<h1>Bye, world!</h1>")
        // add a CC
        |> mail.with_cc(mail.Address(name: None, email: "someone@example.org")),
    )

  echo rcpt

  // Don't forget to close the connection!
  client.close(sock)

  // You can use the convenience method which automatically
  // closes the connection when it goes out of scope
  use res <- client.smtp(opts)
  let assert Ok(sock) = res
  let assert Ok(rcpt) =
    client.deliver_blocking(
      sock,
      mail.make(
        "Hello, world!",
        mail.Address(name: Some("Me"), email: "me@example.com"),
        to: [
          // send to multiple people!
          mail.Address(name: Some("you"), email: "you@example.com"),
          mail.Address(name: Some("you too"), email: "you2@example.com"),
        ],
      )
        |> mail.with_html_body("<h1>Hello, world!</h1>"),
    )

  echo rcpt

  Nil
}
```

Further documentation can be found at <https://hexdocs.pm/gleamail>.

## Development

```sh
gleam run   # Run the above example
gleam test  # Run the tests
```
