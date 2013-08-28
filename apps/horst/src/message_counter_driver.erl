%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(message_counter_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(MAX_QUEUE_LENGTH, 20).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	Data = proplists:get_value(data, Module_config, []),
	Counter = proplists:get_value(counter, Module_config, 0),
	Module_config_1 = lists:keyreplace(data, 1 , Module_config, {data, add(Data, {date:timestamp_to_date(Time), 
		[binary_to_list(Node) ,binary_to_list(Sensor), binary_to_list(Id), date:timestamp_to_date(Time), Body]})}),
	Module_config_2 = lists:keyreplace(counter, 1 , Module_config_1, {counter, Counter + 1}),
	lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_2}).
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
