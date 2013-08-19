%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(dht22_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([call_sensor/1]).

call_sensor(Config) when is_list(Config)->
    Value = call_driver(),
    Msg = sensor:create_message(node(), ?MODULE, sensor:get_id(Config), date:get_date_seconds(), parse_message_from_dht22(Value)),
    lager:debug("got the temp and the humidity from the DHT22 ~p",[Msg]),
    sensor:send_message(nodes(), Msg),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver() ->
    Driver = filename:join([code:priv_dir(horst), "driver", "dht22", "Adafruit_DHT"]),
    os:cmd(Driver ++ " 22 4").

parse_message_from_dht22(Msg) ->
    lager:info("Msg from DHT : ~p", [Msg]),
    Temp = case re:run(Msg ,"Temp =\s+([0-9.]+)") of 
       nomatch -> {temp, 0.0};
       {match,[{C1,C2},{C3,C4}]} -> {temp, string:substr(Msg, C3 + 1, C4)}
    end,
    Hum = case re:run(Msg ,"Hum =\s+([0-9.]+)") of 
       nomatch -> {hum, 0.0};
       {match,[{C11,C22},{C33,C44}]} -> {hum, string:substr(Msg, C33 + 1, C44)}
    end,
    [Temp, Hum].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
parse_message_from_dht22_test() ->
    Msg = "Using pin #4\nData (40): 0x1 0xa7 0x1 0xf 0xb8\nTemp =  27.1 *C, Hum = 42.3 %",
    ?assertEqual([{temp, "27.1"},{hum, "42.3"}], parse_message_from_dht22(Msg)).
-endif.
