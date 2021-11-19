-module(helium_optee_port).
-export([start/1, init/1]).
-export([sign/1, ecdh/1]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

init(ExtPrg) ->
    register(helium_optee_p, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

sign(X) ->
    call_port({sign, X}).

ecdh(Y) ->
    call_port({ecdh, Y}).

call_port(Msg) ->
    helium_optee_p ! {call, self(), Msg},
    receive
        {helium_optee, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg}->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {helium_optee, decode(Data)}
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


encode({sign, X}) -> [1, X];
encode({ecdh, Y}) -> [2, Y].

decode([Int]) ->
     Int.
    
                          
    
