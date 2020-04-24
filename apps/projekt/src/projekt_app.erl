%%%-------------------------------------------------------------------
%% @doc projekt public API
%% @end
%%%-------------------------------------------------------------------

-module(projekt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    projekt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
