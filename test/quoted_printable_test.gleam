import gleam/list
import gleamail/internal/quoted_printable.{encode}
import gleeunit/should

pub fn basic_test() {
  let matrix = [
    #("Hello, world!", "Hello, world!"),
    #("\u{c}\u{d}\u{e}\u{f}", "=0C=0D=0E=0F"),
    #("Some\r\nLine\nBreaks\r", "Some=0D=0ALine=0ABreaks=0D"),
    #(
      "ÕÖð÷ý= Здесь",
      "=C3=95=C3=96=C3=B0=C3=B7=C3=BD=3D =D0=97=D0=B4=D0=B5=D1=81=D1=8C",
    ),
    #(
      "ನಮಸ್ಕಾರ विश्व",
      "=E0=B2=A8=E0=B2=AE=E0=B2=B8=E0=B3=8D=E0=B2=95=E0=B2=BE=E0=B2=B0 =E0=A4=B5=E0=A4=BF=E0=A4=B6=E0=A5=8D=E0=A4=B5",
    ),
    #(
      "きょう、僕は寝ます。　",
      "=E3=81=8D=E3=82=87=E3=81=86=E3=80=81=E5=83=95=E3=81=AF=E5=AF=9D=E3=81=BE=E3=81=99=E3=80=82=E3=80=80",
    ),
  ]

  list.each(matrix, fn(x) {
    let #(in, out) = x
    encode(in)
    |> should.equal(out)
  })
}
