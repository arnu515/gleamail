import gleam/dynamic.{type Dynamic}

pub type OpenError {
  BadOption(OpenBadOption)
  HostFailure(reason: HostFailureReason, host: String, failure: Failure)
}

pub type HostFailureReason {
  Send
  NoMoreHosts
  RetriesExceeded
}

pub type OpenBadOption {
  NoRelay
  InvalidPort
  NoCredentials
}

pub type Failure {
  TemporaryFailure(reason: TemporaryFailureReason)
  PermanentFailure(reason: PermanentFailureReason)
  MissingRequirement(requirement: MissingRequirement)
  UnexpectedResponse(requirement: List(BitArray))
  /// `timeout` or `inet:posix` error
  NetworkFailure(error: Dynamic)
}

pub type TemporaryFailureReason {
  /// could not establish a TLS connection
  TlsFailed
  OtherTemporaryFailureReason(reason: BitArray)
}

pub type PermanentFailureReason {
  AuthFailed
  /// the `ssl` erlang application was not started
  SslNotStarted
  OtherPermanentFailureReason(reason: BitArray)
}

pub type MissingRequirement {
  Auth
  Tls
}
