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
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, handle_msg/3]).

init(Config) ->
	lager:info("~p:init('~p')", [?MODULE, Config]),	
	Table_Id = thing:get_table_id(Config),
	[{rules, Rules}] = ets:lookup(Table_Id, rules),	
	Rules_1 = dsl:create_rules(?MODULE, Rules), 
	thing:save_data_to_ets(Config, funs, Rules_1),	
	{ok, Config}.

%% OS_data = [{temp,38.47}{avg1,0.0390625}{avg5,0.109375}{avg15,0.12890625}]
%% {node,[{temp,38.47}{avg1,0.0390625}{avg5,0.109375}{avg15,0.12890625}]}
handle_msg([Node ,Sensor, Id, Time, OS_data], Config, Module_config) ->	
	Table_Id = thing:get_table_id(Config) ,
	[{data, Data}] = ets:lookup(Table_Id, data),
	[{funs, Funs}] = ets:lookup(Table_Id, funs),
	Data_1 = lists:keystore(Node, 1, Data, {Node, date:timestamp_to_date(Time), OS_data}), 
	thing:save_data_to_ets(Config, Data_1),
	apply_functions(Funs, Node, proplists:lookup(temp, OS_data)),
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

apply_functions(undefined, Node, Data) ->
	lager:info("no rules for module : ~p defined", [?MODULE]);
apply_functions([], Node,  Data) ->
	lager:info("no rules for module : ~p defined", [?MODULE]);
apply_functions(Funs, Node, Data) ->
	%%lager:info("~p : ~p", [Funs, Data]),
	dsl:apply_rules(Funs, Node, Data). 
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.