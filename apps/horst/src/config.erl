%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 20.02.2014

-module(config).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_module_config/1, set_module_config/2]).
-export([get_value/2, get_value/3, get_values/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_module_config(Config) ->
    {driver, {_Module, _Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Module_config.

set_module_config(Config, Module_config) ->
    [].

get_level_values(Level, Keys, List_of_tuples) ->
    List_of_tuples_1 = get_level(Level, List_of_tuples),
    get_values(Keys, List_of_tuples_1).

get_level([], List_of_tuples) ->
    List_of_tuples;
get_level([Key|Keys], List_of_tuples) ->
    get_level(Keys, get_value(Key, List_of_tuples)).

get_values(Keys, List_of_tuples) ->
    get_values(Keys, List_of_tuples, []). 

get_values([], List_of_tuples, Acc) ->
    lists:reverse(Acc);
get_values([Key|Keys], List_of_tuples, Acc) ->
    get_values(Keys, List_of_tuples, [get_value(Key, List_of_tuples)|Acc]).

get_value(Key, List_of_tuples) ->
    proplists:get_value(Key, List_of_tuples).
get_value(Key, List_of_tuples, Default) ->
    proplists:get_value(Key, List_of_tuples, Default).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_level_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, [{sub3_k1, "v31"}, {sub3_k2, "v32"}]}, {key4, "v4"}],
    ?assertEqual(["v31"], get_level_values([key3], [sub3_k1], List_of_tuples)).

get_values_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, "v3"}, {key4, "v4"}],
    ?assertEqual(["v2", "v4"], get_values([key2, key4], List_of_tuples)).
    
get_module_config_test() ->

    Config = 
     [{id, "0"},
      {activ, false},
     {driver, {fungi_driver,handle_msg},
        [{data,[{window,close}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}]}],

    Result = [{data,[{window,close}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}],

    ?assertEqual(Result, get_module_config(Config)).

set_module_config_test() ->
    ok.
-endif.