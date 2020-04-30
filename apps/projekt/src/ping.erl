
-module(ping).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

-define(SERVER, ?MODULE).


start_link(PingId) ->
  gen_server:start_link({local, PingId}, ?MODULE, [], []).

init([]) ->
  {ok, {}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({pong, _Request}, State) ->
  io:format("receive pong msg"),
  io:format("~s", [_Request]),
  {noreply, State};

handle_cast({ping, _Request}, State) ->
  io:format("send ping msg"),
  io:format("~s", [_Request]),
  gen_server:cast(port1, {start, _Request}),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
