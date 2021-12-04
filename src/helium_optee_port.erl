-module(helium_optee_port).
-export([start/0, start/1, init/1, stop/0]).
-export([sign/1, ecdh/1, gen_ecdsa_keypair/0]).

start() ->
    start("priv/helium-optee-erl-port").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    helium_optee_p ! stop.

init(ExtPrg) ->
    register(helium_optee_p, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, nouse_stdio]),
    loop(Port).

sign(X) ->
    call_port({sign, X}).

ecdh(Y) ->
    call_port({ecdh, Y}).

gen_ecdsa_keypair() ->
    call_port({gen_ecdsa_keypair}).

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


encode({gen_ecdsa_sign}) -> [1, 0];
encode({ecdsa_sign, X}) -> [2, X];
encode({ecdh, Y}) -> [3, Y].

decode([Int]) ->
    io:format("Received data:[Int]~n"),
    Int;
decode([]) ->
    io:format("Received data:[]~n"),
    0;
decode(Int) ->
    io:format("Received data:Int~n"),
    Int.



    
                          
    
