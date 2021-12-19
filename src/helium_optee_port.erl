-module(helium_optee_port).
-export([start/0, start/1, init/1, stop/0]).
-export([ecdh/1, gen_ecdsa_keypair/0, ecdsa_sign/1]).

-define(REPLY, 0).

start() ->
    start("priv/helium-optee-erl-port").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    helium_optee_p ! stop.

init(ExtPrg) ->
    register(helium_optee_p, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, nouse_stdio, binary]),
    loop(Port).

ecdh(Y) ->
    call_port({ecdh, Y}).

gen_ecdsa_keypair() ->
    call_port({gen_ecdsa_keypair}).

ecdsa_sign(X) ->
    call_port({ecdsa_sign, X}).

call_port(Msg) ->
    helium_optee_p ! {call, self(), Msg},
    receive
        {helium_optee, Result} ->
            io:format("call_port: ~w~n", [Result]),
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

