
-module(ports_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, pingportSupervisor}, ?MODULE, []).

init([]) ->
  Ping1 = {ping1, {ping, start_link, [ping1]}, permanent, 2000, worker, [ping]},
  Port1 = {port1, {port, start_link, [port1]}, permanent, 2000, worker, [port]},
  Port2 = {port2, {port, start_link, [port2]}, permanent, 2000, worker, [port]},
  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [Ping1, Port1, Port2]}
  }.

%% ports_supervisor:start_link().

%% gen_server:cast(ping1, {ping, ala}).