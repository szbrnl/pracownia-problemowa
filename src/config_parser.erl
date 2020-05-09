
-module(config_parser).

%% API
-export([parse_config/0]).

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
  after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof ->
      io:format(ups),
      [];
    Line -> string:trim(Line) ++ get_all_lines(Device)
  end.

parse_config() ->
  Conf = readlines("program.json"),
  jiffy:decode(Conf, [return_maps]).

%%  ports_supervisor:start_link().
