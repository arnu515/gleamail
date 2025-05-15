import gleam/bool
import gleam/int
import gleam/list
import gleam/string

fn quot_ascii(x: Int) -> List(Int) {
  case x {
    // allow \t and printable ASCII characters (excluding =)
    x if x == 0x09 || x >= 0x20 && x <= 0x7E && x != 61 -> [x]
    x ->
      { "=" <> int.to_base16(x) |> string.pad_start(to: 2, with: "0") }
      |> string.to_utf_codepoints
      |> list.map(string.utf_codepoint_to_int)
  }
}

pub fn encode(str: String) -> String {
  let to_base16 = fn(x) {
    int.to_base16(x) |> string.pad_start(to: 2, with: "0")
  }

  string.to_utf_codepoints(str)
  |> list.flat_map(fn(x) {
    let y = string.utf_codepoint_to_int(x)
    use <- bool.guard(
      y < 0b01_11_11_11,
      list.map(quot_ascii(y), fn(x) {
        let assert Ok(x) = string.utf_codepoint(x)
        x
      }),
    )
    use <- bool.lazy_guard(y < 0b1_11_11_11_11_11, fn() {
      string.to_utf_codepoints(
        "="
        <> to_base16(
          int.bitwise_shift_right(y, 6) |> int.bitwise_or(0b11_00_00_00),
        )
        <> "="
        <> to_base16(
          int.bitwise_and(y, 0b00_11_11_11) |> int.bitwise_or(0b10_00_00_00),
        ),
      )
    })
    use <- bool.lazy_guard(y < 0b11_11_11_11_11_11_11_11, fn() {
      string.to_utf_codepoints(
        "="
        <> to_base16(
          int.bitwise_shift_right(y, 12) |> int.bitwise_or(0b11_10_00_00),
        )
        <> "="
        <> to_base16(
          int.bitwise_shift_right(y, 6)
          |> int.bitwise_and(0b00_11_11_11)
          |> int.bitwise_or(0b10_00_00_00),
        )
        <> "="
        <> to_base16(
          int.bitwise_and(y, 0b00_11_11_11)
          |> int.bitwise_or(0b10_00_00_00),
        ),
      )
    })
    string.to_utf_codepoints(
      "="
      <> to_base16(
        int.bitwise_shift_right(y, 18) |> int.bitwise_or(0b11_11_00_00),
      )
      <> "="
      <> to_base16(
        int.bitwise_shift_right(y, 12)
        |> int.bitwise_and(0b00_11_11_11)
        |> int.bitwise_or(0b10_00_00_00),
      )
      <> "="
      <> to_base16(
        int.bitwise_shift_right(y, 6)
        |> int.bitwise_and(0b00_11_11_11)
        |> int.bitwise_or(0b10_00_00_00),
      )
      <> "="
      <> to_base16(
        int.bitwise_and(y, 0b00_11_11_11)
        |> int.bitwise_or(0b10_00_00_00),
      ),
    )
  })
  // TODO: lines max size is 76 chars, using = as short line breaks
  // TODO: encode 0x20 or 0x09 before a \r\n instead of printing it as is
  |> string.from_utf_codepoints
}
