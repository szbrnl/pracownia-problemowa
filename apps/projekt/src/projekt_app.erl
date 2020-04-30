%%%-------------------------------------------------------------------
%% @doc projekt public API
%% @end
%%%-------------------------------------------------------------------

-module(projekt_app).

-behaviour(gen_server).

-export([start_link/1, init/1]).

start_link(InitialValue) ->
    gen_server:start_link(
        {local,var_server},
        var_server,
        InitialValue, []).

init(InitialValue) ->




    {ok, InitialValue}.

%% internal functions
