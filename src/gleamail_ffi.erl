-module(gleamail_ffi).
-export([smtpopts_to_gensmtp_options/1]).

% converts SmtpOpts into https://hexdocs.pm/gen_smtp/gen_smtp_client.html#t:options/0
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

%% Utils

%% append_if_some(List, none) -> List;
%% append_if_some(List, {some, X}) -> [X|List].
append_if_some(List, none, _F) -> List;
append_if_some(List, {some, X}, F) -> [F(X)|List].
