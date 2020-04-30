%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(packet_pipeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(packet_pipeline_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(PacketPipelineId) ->
  gen_server:start_link({local, PacketPipelineId}, ?MODULE, [], []).

init([]) ->
  {ok, #packet_pipeline_state{}}.

handle_call({PortId, Packet}, _From, State = #packet_pipeline_state{}) ->
  {reply, ok, State}.

handle_cast({PortId, Packet}, State = #packet_pipeline_state{}) ->
%%  do operation over packet
  gen_server:cast(PortId, Packet),
  {noreply, State}.

handle_info(_Info, State = #packet_pipeline_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #packet_pipeline_state{}) ->
  ok.

code_change(_OldVsn, State = #packet_pipeline_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
