
-module(port).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link(PortId) ->
  io:format(PortId),
  gen_server:start_link({local, PortId}, ?MODULE, PortId, []).

init(PortId) ->
  {ok, PortId}.

handle_call(_Request, _From, PortId) ->
  io:format(port),
  io:format(_Request),
  {reply, ok, PortId}.

handle_cast({start, _Request}, PortId) ->
  io:format(PortId),
  spawn_link(packet_pipeline, process_packet, [_Request, PortId]),
  {noreply, PortId};

handle_cast({processed, _Request}, PortId) ->
  io:format(PortId),
  io:nl(),
  gen_server:cast(ping1, {pong, _Request}),
  {noreply, PortId}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
