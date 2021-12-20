-module(helium_optee).

-include_lib("public_key/include/public_key.hrl").

-export([start/0, start/1, init/1, stop/0]).
-export([start_link/0]).
%% this are for directly calling
-export([gen_ecdh_keypair/0, ecdh/1, gen_ecdsa_keypair/0, ecdsa_sign/1]).
%% these are for library calling (passing Pid as first argument)
-export([gen_ecdh_keypair/1, ecdh/2, gen_ecdsa_keypair/1, ecdsa_sign/2]).
-export([stop/1]).

-define(REPLY, 0).

- spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start().

start() ->
    start(code:priv_dir(helium_optee) ++ "/helium_optee_port").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

-spec stop(pid()) -> ok | {error, term}.
stop(Pid) ->
    stop().

stop() ->
    call_port({optee_stop, []}).

init(ExtPrg) ->
    register(helium_optee_p, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, nouse_stdio, binary]),
    loop(Port).

ecdh(Pid, {#'ECPoint'{point=PubPoint}, _}) ->
    << _:8, X:32/binary, Y:32/binary>> = PubPoint,
    ecdh(X, Y).

ecdh({X, Y}) ->
    call_port({ecdh, {X, Y}}).

-spec gen_ecdsa_keypair(pid()) -> ok | {error, term()}.
gen_ecdsa_keypair(Pid) ->
    gen_ecdsa_keypair().

-spec gen_ecdsa_keypair() -> ok | {error, term()}.
gen_ecdsa_keypair() ->
    call_port({gen_ecdsa_keypair}).

-spec gen_ecdh_keypair(pid()) -> {ok} | {error, term()}.
gen_ecdh_keypair(Pid) ->
    gen_ecdh_keypair().

-spec gen_ecdh_keypair() -> ok | {error, term()}.
gen_ecdh_keypair() ->
    call_port({gen_ecdh_keypair}).

-spec ecdsa_sign(pid(), binary()) -> {ok, Signature::binary()} | {error, term()}.
ecdsa_sign(Pid, Digest) ->
    ecdsa_sign(Digest).

-spec ecdsa_sign(binary()) -> {ok, Signature::binary()} | {error, term()}.
ecdsa_sign(Digest) ->
    call_port({ecdsa_sign, Digest}).

call_port(Msg) ->
    helium_optee_p ! {call, self(), Msg},
    receive
        {helium_optee, Result} ->
            io:format("call_port Result: ~w~n", [Result]),
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg}->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, <<?REPLY, Response/binary>>}} ->
                    io:format("~w response: ~w~n", [Msg, Response]),
                    Caller ! {helium_optee, binary_to_term(Response)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit(port_terminated)
    end.

