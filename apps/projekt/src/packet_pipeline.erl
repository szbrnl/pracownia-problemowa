-module(packet_pipeline).

-export([process_packet/2]).

process_packet(Packet, PortId) ->
  io:format("processing packet"),
  gen_server:cast(PortId, {processed, Packet}),
  ok.