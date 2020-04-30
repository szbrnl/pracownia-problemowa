%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ports_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, pingportSupervisor}, ?MODULE, []).

init([]) ->
  Ping1 = {ping1, {ping_final, start_link, [ping1]}, permanent, 2000, worker, [ping_final]},
  Port1 = {port1, {port_final, start_link, [port1]}, permanent, 2000, worker, [port_final]},
  Port2 = {port2, {port_final, start_link, [port2]}, permanent, 2000, worker, [port_final]},
  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [Ping1, Port1, Port2]}
  }.

%% ports_supervisor:start_link().

%% gen_server:call(port2, 1).

%%gen_server:cast(ping1, {ping, ala}).