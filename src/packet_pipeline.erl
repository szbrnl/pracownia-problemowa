-module(packet_pipeline).

-export([process_packet/2, process_packet/3]).

-define(Headers, <<"headers">>).
-define(Metadata, <<"metadata">>).
-define(Header_types, <<"header_types">>).
-define(Header_type, <<"header_type">>).
-define(Name, <<"name">>).
-define(Fields, <<"fields">>).
-define(Actions, <<"actions">>).
-define(Primitives, <<"primitives">>).
-define(Op, <<"op">>).
-define(Assign, <<"assign">>).
-define(Parameters, <<"parameters">>).
-define(Value, <<"value">>).

process_packet(Packet, PortId) ->
  Binary = process_packet_fun(Packet),
  gen_server:cast(PortId, {processed, Binary}),
  ok.

process_packet(Packet, PortId, Callback) ->
  Processed = process_packet_fun(Packet),
  gen_server:cast(PortId, {processed, Processed, Callback}).

process_packet_fun(Packet) ->
  io:format("processing packet"),
  Parsed = parse_packet(Packet),
  error_logger:info_report([parsed, Parsed]),
  Processed = process_packet(Parsed),
  Binary = packet_to_bin(Processed),
  Binary.

packet_to_bin(Packet) ->
  FieldsOrder = maps:get(fields_order, Packet),
  [{_, Data}] = maps:to_list(maps:remove(fields_order, Packet)),
  Result = lists:foldl(fun(Name, Binary) -> Binary ++ [maps:get(Name, Data)] end, [], FieldsOrder),
  Result.

parse_packet(Packet) ->
  Config = config_parser:parse_config(),
  create_packet_with_struct(Config, Packet).

create_packet_with_struct(Config, Packet) ->
  Headers = maps:get(?Headers, Config),
  Header_types = maps:get(?Header_types, Config),

  PacketStruct = lists:filter(fun(E) ->
    Metadata = maps:get(?Metadata, E),
    if
      Metadata == false -> true;
      true -> false
    end
                              end, Headers),

  Header_types = maps:get(?Header_types, Config),
  fill_struct_with_packet(PacketStruct, #{fields_order => []}, Header_types, Packet).

fill_struct_with_packet([], Acc, _, _) ->
  Acc;

fill_struct_with_packet([H | T], Map, Header_types, Packet) ->
  Header_type = maps:get(?Header_type, H),
  [Fields | _] = lists:filter(fun(E) ->
    Name = maps:get(?Name, E),
    if
      Name == Header_type -> true;
      true -> false
    end
                              end, Header_types),

  FieldsList = maps:get(?Fields, Fields),
  {FieldsMap, PacketR} = lists:foldl(fun([Name, Size, _], {Acc, PacketRest}) ->
    if
      (bit_size(PacketRest) =< Size) ->
        {Acc#{Name => PacketRest}, <<>>};
      true ->
        BinarySize = Size div 8,
        <<PacketRestH: BinarySize/binary, PacketRestT/binary>> = PacketRest,
        UpdatedAcc = Acc#{Name => PacketRestH},
        {UpdatedAcc, PacketRestT}
    end
                                     end, {#{}, Packet}, FieldsList),

  UpdatedWithFields = Map#{maps:get(?Name, H) => FieldsMap},
  FieldsListNames = lists:map(fun([Name, _, _]) -> Name end, FieldsList),
  UpdatedWithFieldsOrder = UpdatedWithFields#{fields_order => maps:get(fields_order, UpdatedWithFields) ++ FieldsListNames},
  fill_struct_with_packet(T, UpdatedWithFieldsOrder, Header_types, PacketR).


process_packet(Packet_with_struct) ->
  Config = config_parser:parse_config(),
  [T | _] = maps:get(?Actions, Config),
  OpList = maps:get(?Primitives, T),
  Scalars = #{},
  assign_op(Packet_with_struct, Scalars, OpList).

assign_op(Packet_with_struct, _, []) ->
  Packet_with_struct;

assign_op(Packet_with_struct, Scalars, [Op | OpList]) ->
  OpName = maps:get(?Op, Op),
  if
    OpName /= ?Assign -> assign_op(Packet_with_struct, Scalars, OpList);
    OpName == ?Assign ->
      [Param1, Param2] = maps:get(?Parameters, Op),
      {NewPacket, ScalarsUp} = do_assign_op(Packet_with_struct, Scalars, Param1, Param2),
      assign_op(NewPacket, ScalarsUp, OpList)
  end.

do_assign_op(Packet_with_struct, Scalars, Param1, Param2) ->
  [V1A, V1B] = maps:get(?Value, Param1),
  [V2A, V2B] = maps:get(?Value, Param2),

  {Tmp, Scalars2} = get_field_value(V2A, V2B, Packet_with_struct, Scalars),

  {PacketUp, ScalarsUp} = update_field(Tmp, V1A, V1B, Packet_with_struct, Scalars2),

  {PacketUp, ScalarsUp}.

get_field_value(VA, VB, Packet_with_struct, Scalars) ->
  KeyInPacket = maps:is_key(VA, Packet_with_struct),
  KeyInScalar = maps:is_key(VB, Scalars),
  if
    KeyInPacket -> {maps:get(VB, maps:get(VA, Packet_with_struct)), Scalars};
    KeyInScalar -> {maps:get(VB, Scalars), Scalars};
    true -> {<<"0">>, Scalars#{VB => <<"0">>}}
  end.

update_field(NewValue, VA, VB, Packet_with_struct, Scalars) ->
  KeyInPacket = maps:is_key(VA, Packet_with_struct),
  if
    KeyInPacket ->
      VAUpdated = (maps:get(VA, Packet_with_struct))#{VB => NewValue},
      PacketUp = Packet_with_struct#{VA => VAUpdated},
      {PacketUp, Scalars};
    true ->
      {Packet_with_struct, Scalars#{VB => NewValue}}
  end.