import gleam/dynamic
import gleam/erlang/atom.{create_from_string as atom}
import gleam/erlang/charlist.{from_string as charlist}
import gleamail/error
import gleeunit/should

pub type HostFailure {
  HostFailure(
    reason: error.HostFailureReason,
    host: String,
    failure: error.Failure,
  )
}

@external(erlang, "gleamail_ffi", "hostname_to_string")
fn hostname_to_string(h: a) -> String

@external(erlang, "gleamail_ffi", "decode_host_failure")
fn decode_host_failure(r: atom.Atom, h: a) -> HostFailure

@external(erlang, "gleamail_ffi", "decode_failure")
fn decode_failure(h: a) -> error.Failure

pub fn hostname_to_string_test() {
  charlist("hello")
  |> hostname_to_string
  |> should.equal("hello")

  atom("hello")
  |> hostname_to_string
  |> should.equal("hello")
}

pub fn decode_host_failure_test() {
  let assert Ok(reason) = atom.from_dynamic(dynamic.from(error.Send))
  let host = charlist("Some Host")
  let host_failure =
    HostFailure(
      error.Send,
      charlist.to_string(host),
      error.UnexpectedResponse([]),
    )
  #(atom("temporary_failure"), host, atom("tls_failed"))
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(
      ..host_failure,
      failure: error.TemporaryFailure(error.TlsFailed),
    ),
  )
  #(atom("temporary_failure"), host, <<"oopsie">>)
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(
      ..host_failure,
      failure: error.TemporaryFailure(
        error.OtherTemporaryFailureReason(<<"oopsie">>),
      ),
    ),
  )

  #(atom("permanent_failure"), host, atom("auth_failed"))
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(
      ..host_failure,
      failure: error.PermanentFailure(error.AuthFailed),
    ),
  )
  #(atom("permanent_failure"), host, atom("ssl_not_started"))
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(
      ..host_failure,
      failure: error.PermanentFailure(error.SslNotStarted),
    ),
  )
  #(atom("permanent_failure"), host, <<"oopsie">>)
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(
      ..host_failure,
      failure: error.PermanentFailure(
        error.OtherPermanentFailureReason(<<"oopsie">>),
      ),
    ),
  )

  #(atom("missing_requirement"), host, atom("auth"))
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(..host_failure, failure: error.MissingRequirement(error.Auth)),
  )
  #(atom("missing_requirement"), host, atom("tls"))
  |> decode_host_failure(reason, _)
  |> should.equal(
    HostFailure(..host_failure, failure: error.MissingRequirement(error.Tls)),
  )
}

pub fn decode_failure_test() {
  #(atom("temporary_failure"), atom("tls_failed"))
  |> decode_failure
  |> should.equal(error.TemporaryFailure(error.TlsFailed))
  #(atom("temporary_failure"), <<"oopsie">>)
  |> decode_failure
  |> should.equal(
    error.TemporaryFailure(error.OtherTemporaryFailureReason(<<"oopsie">>)),
  )

  #(atom("permanent_failure"), atom("auth_failed"))
  |> decode_failure
  |> should.equal(error.PermanentFailure(error.AuthFailed))
  #(atom("permanent_failure"), atom("ssl_not_started"))
  |> decode_failure
  |> should.equal(error.PermanentFailure(error.SslNotStarted))
  #(atom("permanent_failure"), <<"oopsie">>)
  |> decode_failure
  |> should.equal(
    error.PermanentFailure(error.OtherPermanentFailureReason(<<"oopsie">>)),
  )

  #(atom("missing_requirement"), atom("auth"))
  |> decode_failure
  |> should.equal(error.MissingRequirement(error.Auth))
  #(atom("missing_requirement"), atom("tls"))
  |> decode_failure
  |> should.equal(error.MissingRequirement(error.Tls))
}
