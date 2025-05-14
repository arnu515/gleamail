-module(gleamail_ffi).
-export([smtpopts_to_gensmtp_options/1, gen_smtp_client_open/1, get_mimetuple/4]).

%% for testing purposes only
-export([hostname_to_string/1, decode_host_failure/2, decode_failure/1]).

%% converts SmtpOpts into https://hexdocs.pm/gen_smtp/gen_smtp_client.html#t:options/0
smtpopts_to_gensmtp_options({smtp_opts, Ssl, Tls, Port, Timeout, Relay, NoMx, Auth, Hostname, Retries, Username, Password, QuitOnTxErr}) ->
    Opts = append_if_some(append_if_some(
        append_if_some(
            append_if_some(append_if_some(append_if_some([
                {ssl, Ssl},
                {tls, Tls},
                {port, Port},
                {no_mx_lookups, NoMx},
                {auth, Auth},
                {retries, Retries}
            ], Username, fun(X) -> {username, unicode:characters_to_list(X)} end),
            Password, fun(X) -> {password, unicode:characters_to_list(X)} end),
            Timeout, fun(X) -> {timeout, X} end),
            Hostname, fun(X) -> {hostname, unicode:characters_to_list(X)} end
        ),
        QuitOnTxErr,
        fun (X) -> {on_transaction_error, case X of
            true -> quit;
            false -> reset end} end
    ), Relay, fun(X) -> 
        {relay, case X of
            {hostname, Y} -> unicode:characters_to_list(Y);
            {ipv4_address, A, B, C, D} -> {A, B, C, D};
            {ipv6_address, A, B, C, D, E, F, G, H} -> {A, B, C, D, E, F, G, H}
        end} end
    ),
    io:format("~w~n", [Opts]),
    Opts.

gen_smtp_client_open(Opts) ->
    case gen_smtp_client:open(Opts) of
        {ok, Sock} -> {ok, Sock};
        {error, bad_option, X} -> {error, {bad_option, X}};
        {error, no_more_hosts = Reason, X} -> decode_host_failure(Reason, X);
        {error, send = Reason, X} -> decode_host_failure(Reason, X);
        {error, retries_exceeded = Reason, X} -> decode_host_failure(Reason, X)
    end.

%% https://hexdocs.pm/gen_smtp/mimemail.html#t:mimetuple/0
get_mimetuple(Type, Subtype, Headers, [Body]) when is_binary(Body) -> 
    {Type, Subtype, Headers, #{}, Body};
get_mimetuple(Type, Subtype, Headers, [Plain, Html]) when is_binary(Plain) andalso is_binary(Html) -> 
    {Type, Subtype, Headers, #{},
        [{<<"text">>, <<"plain">>, [], #{ content_type_params => [{<<"charset">>, <<"utf-8">>}] }, Plain},
        {<<"text">>, <<"html">>, [], #{ content_type_params => [{<<"charset">>, <<"utf-8">>}] }, Html}]
    }.

%% Utils

%% append_if_some(List, none) -> List;
%% append_if_some(List, {some, X}) -> [X|List].
append_if_some(List, none, _F) -> List;
append_if_some(List, {some, X}, F) -> [F(X)|List].

decode_host_failure(Reason, X) ->
    case X of
        {temporary_failure = F, Host, tls_failed = R} -> {host_failure, Reason, hostname_to_string(Host), {F, R}};
        {permanent_failure = F, Host, auth_failed = R} -> {host_failure, Reason, hostname_to_string(Host), {F, R}};
        {permanent_failure = F, Host, ssl_not_started = R} -> {host_failure, Reason, hostname_to_string(Host), {F, R}};
        {temporary_failure = F, Host, R} when is_binary(R) -> {host_failure, Reason, hostname_to_string(Host), {F, {other_temporary_failure_reason, R}}};
        {permanent_failure = F, Host, R} when is_binary(R) -> {host_failure, Reason, hostname_to_string(Host), {F, {other_permanent_failure_reason, R}}};
        {missing_requirement = F, Host, auth = R} -> {host_failure, Reason, hostname_to_string(Host), {F, R}};
        {missing_requirement = F, Host, tls = R} -> {host_failure, Reason, hostname_to_string(Host), {F, R}};
        {unexpected_response = F, Host, ListOfBinary} -> {host_failure, Reason, hostname_to_string(Host), {F, ListOfBinary}};
        {network_failure = F, Host, {error, Error}} -> {host_failure, Reason, hostname_to_string(Host), {F, Error}}
    end.

decode_failure(X) ->
    case X of
        {temporary_failure = F, tls_failed = R} -> {F, R};
        {permanent_failure = F, auth_failed = R} -> {F, R};
        {permanent_failure = F, ssl_not_started = R} -> {F, R};
        {temporary_failure = F, R} when is_binary(R) -> {F, {other_temporary_failure_reason, R}};
        {permanent_failure = F, R} when is_binary(R) -> {F, {other_permanent_failure_reason, R}};
        {missing_requirement = F, auth = R} -> {F, R};
        {missing_requirement = F, tls = R} -> {F, R};
        {unexpected_response = F, ListOfBinary} -> {F, ListOfBinary};
        {network_failure = F, {error, Error}} -> {F, Error}
    end.

%% A hostname can be a charlist or an atom
hostname_to_string(H) when is_list(H) -> unicode:characters_to_binary(H);
hostname_to_string(H) when is_atom(H) -> unicode:characters_to_binary(atom_to_list(H)).
