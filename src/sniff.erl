
-module(sniff).

-behaviour(gen_statem).

-include_lib("pkt/include/pkt.hrl").

% Interface
-export([start/0, start/1, stop/0]).

-export([start_link/0]).

% States
-export([waiting/3, sniffing/3]).

% Behaviours
-export([init/1, callback_mode/0, handle_info/3, terminate/3, code_change/4]).

-define(is_print(C), C >= $\s, C =< $~).

-record(state,
        {pid, crash = true,
         format = []}). % full packet dump: binary, hex

%%--------------------------------------------------------------------
%%% Interface
%%--------------------------------------------------------------------
start_link() -> gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

start() -> start([{filter, "tcp and port 80"}, {chroot, "priv/tmp"}, {inject, true}]).

start(Opt) when is_list(Opt) -> gen_statem:cast(?MODULE, {start, Opt}).

stop() -> gen_statem:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
callback_mode() -> state_functions.

init([]) -> process_flag(trap_exit, true), {ok, waiting, #state{}}.

%%
%% State: sniffing
%%
handle_info({packet, DLT, Time, Len, Data}, sniffing,
            #state{pid = Pid, format = Format} = State) ->
    Callback = fun(Packet)-> epcap:send(Pid, Packet)  end,
    gen_server:cast(port1, {start, Data, Callback}),
    {next_state, sniffing, State};

% epcap port stopped
handle_info({'EXIT', _Pid, normal}, sniffing, State) ->
    {next_state, sniffing, State};
%%
%% State: waiting
%%
% epcap port stopped
handle_info({'EXIT', _Pid, normal}, waiting, State) ->
    {next_state, waiting, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% States
%%--------------------------------------------------------------------
waiting(cast, {start, Opt}, State) ->
    Format = proplists:get_value(format, Opt, []),
    Snaplen = proplists:get_value(snaplen, Opt),
    {ok, Pid} = epcap:start_link(Opt),
    {next_state, sniffing,
     State#state{pid = Pid, format = Format, crash = Snaplen =:= undefined}};
waiting(info, Event, State) -> handle_info(Event, waiting, State).

sniffing(cast, {start, Opt}, #state{pid = Pid} = State) ->
    epcap:stop(Pid),
    {ok, Pid1} = epcap:start_link(Opt),
    {next_state, sniffing, State#state{pid = Pid1}};
sniffing(cast, stop, #state{pid = Pid} = State) ->
    epcap:stop(Pid), {next_state, waiting, State};
sniffing(info, Event, State) -> handle_info(Event, sniffing, State).
