%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(port_final).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%%-define(SERVER, ?MODULE).

-record(port_final_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(PortId) ->
  gen_server:start_link({local, PortId}, ?MODULE, [], []).

init([]) ->
  {ok, #port_final_state{}}.

handle_call(_Request, _From, State = #port_final_state{}) ->
  io:format(port),
  io:format(_Request),
  {reply, ok, State}.

handle_cast({Pid, _Request}, State = #port_final_state{}) ->
  io:format(port),
  io:format("~s", [_Request]),
  gen_server:cast(Pid, {pong, _Request}),
  {noreply, State}.

handle_info(_Info, State = #port_final_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #port_final_state{}) ->
  ok.

code_change(_OldVsn, State = #port_final_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
