import gleam/string
import gleamail/mail
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn mail_header_fold_test() {
  mail.fold("Hello, world!", 4, -2)
  |> should.equal(
    ["Hell", "o, ", "wor", "ld!"]
    |> string.join("\r\n "),
  )
}
