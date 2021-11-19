%%%-------------------------------------------------------------------
%% @doc helium_optee_port public API
%% @end
%%%-------------------------------------------------------------------

-module(helium_optee_port_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    helium_optee_port_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
