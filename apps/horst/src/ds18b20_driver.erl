%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(ds18b20_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, call_sensor/2]).

init(Config) ->
    lager:info("ds18b20_driver:init('~p')", [Config]),  
    os:cmd("modprobe w1-gpio"),
    os:cmd("modprobe w1-therm"),
    {ok, Config}.

call_sensor(Config, Module_config) ->
    Temp_line = call_driver(),
    Value = parse_message(Temp_line),
    Msg = sensor:create_message(node(), ?MODULE, config_handler:get_id(Config), Value),
    sensor:send_message(Msg),
    thing:save_data_to_ets(Config, Value),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver() ->
  [W1_Slave] = filelib:wildcard("/sys/bus/w1/devices/28*/w1_slave"),   
  {ok, File} = file:open(W1_Slave, [read]),
  {ok, Yes_line} = file:read_line(File),
  timer:sleep(200), 
  {ok, Temp_line} = file:read_line(File),
  file:close(File),
  Temp_line.   

parse_message(Msg) ->
    lager:info("Msg from ds18b20 : ~p", [Msg]),
    case re:run(Msg ,"t=([0-9.]+)") of 
       nomatch -> {temp, 0.0};
       {match,[{C1,C2},{C3,C4}]} -> {temp, erlang:list_to_integer(string:substr(Msg, C3 + 1, C4))/1000}
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
parse_message_test() ->
    Msg = "37 01 4b 46 7f ff 09 10 26 : crc=26 YES\n 37 01 4b 46 7f ff 09 10 26 t=19437\n",
    ?assertEqual({temp, "19437"}, parse_message(Msg)).
-endif.
