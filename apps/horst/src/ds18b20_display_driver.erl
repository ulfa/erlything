%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(ds18b20_display_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
-define(MAX_QUEUE_LENGTH, 30).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, {temp, Temp}], Config, Module_config) ->
	Table_Id = proplists:get_value(?TABLE, Config),
	[{data, Data}] = ets:lookup(Table_Id, data),
	ets:insert(Table_Id, [{data, add(Data, {Time, Temp})}]),
	Config;

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	lager:warning("~p got the wrong message : ~p", [?MODULE, [Node ,Sensor, Id, Time, Body]]),
	Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
add([], Value) ->
    [Value];
add(List, Value) ->
    case length(List) =< ?MAX_QUEUE_LENGTH of
        true -> [Value|List];
        false -> [Value|lists:sublist(List, ?MAX_QUEUE_LENGTH)]
    end.    

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
