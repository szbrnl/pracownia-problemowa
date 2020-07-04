
-module(sniff_simple).

%% API
-export([start/0, startTmp/0]).

start() ->
  spawn_link(sniff_simple, startTmp, []).

startTmp() ->
  {ok, Ref} = epcap:start_link([{interface, "veth8"}, {inject, true}]),
  resend(Ref).

resend(Ref) ->
  receive
    {packet, _DataLinkType, _Time, _Length, Packet} ->
      Callback = fun(P)-> epcap:send(Ref, P)  end,
      gen_server:cast(port1, {start, Packet, Callback}),
      resend(Ref)
  end.