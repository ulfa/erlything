%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(os_display_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
-define(MAX_QUEUE_LENGTH, 19).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

%% OS_data = [{temp,38.47}{avg1,0.0390625}{avg5,0.109375}{avg15,0.12890625}]
%% {node,[{temp,38.47}{avg1,0.0390625}{avg5,0.109375}{avg15,0.12890625}]}
handle_msg([Node ,Sensor, Id, Time, OS_data], Config, Module_config) ->
	
	Table_Id = thing:get_table_id(Config) ,
	[{data, Data}] = ets:lookup(Table_Id, data),
	Data_1 = lists:keystore(Node, 1, Data, {Node,  OS_data}), 
	thing:save_data_to_ets(Config, Data_1),

	Config;

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	lager:warning("os_display_driver got the wrong message : ~p", [[Node ,Sensor, Id, Time, Body]]),
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