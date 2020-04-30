%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ping_final).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(ping_final_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(PingId) ->
  gen_server:start_link({local, PingId}, ?MODULE, [], []).

init([]) ->
  {ok, #ping_final_state{}}.

handle_call(_Request, _From, State = #ping_final_state{}) ->
  {reply, ok, State}.

handle_cast({pong, _Request}, State = #ping_final_state{}) ->
  io:format("receive pong msg"),
  io:format("~s", [_Request]),
  {noreply, State};

handle_cast({ping, _Request}, State = #ping_final_state{}) ->
  io:format("send ping msg"),
  io:format("~s", [_Request]),
  gen_server:cast(port1, {self(), _Request}),
  {noreply, State}.

handle_info(_Info, State = #ping_final_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #ping_final_state{}) ->
  ok.

code_change(_OldVsn, State = #ping_final_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
