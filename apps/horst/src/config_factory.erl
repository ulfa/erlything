%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(config_factory).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_thing/2]).

create_thing(Name, [{actor, Value}, {driver, {Driver, Function}, Module_Config}]) ->
    Config = get_config(),
    Config_1 = case is_entry(Name, Config) of 
        true -> lager:info("Entry with name : ~p already exists!", [Name]);
        false -> create_entries(Name, [{actor, Value}, {driver, {Driver, Function}, Module_Config}])
    end,
    {thing, Name, Config_1}.


create_entries(Name, [{actor, Value}, {driver, {Driver, Function}, Module_Config}]) ->
    [create_entry(Entry, []) || Entry <- [id, icon, ets, activ, timer, database]].
    

get_config() ->
    {Node, Config} = node_config:get_things_config(),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
is_entry(Name, Config) ->
    lists:keymember(Name, 2, Config).     

create_entry(actor, true) ->
    {actor, true};
create_entry(actor, false) ->
    {actor, false};    
create_entry(actor, Value) ->
    {actor, false};
    
create_entry(id, []) ->
    create_entry(id, "default");
create_entry(id, Value) ->
    {id, Value};

create_entry(icon, []) ->
    create_entry(icon, "thing.png");
create_entry(icon, Value) ->
    {icon, Value};

create_entry(ets, []) ->
    create_entry(ets, false);
create_entry(ets, true) ->
    {ets, true};
create_entry(ets, false) ->
    {ets, false};

create_entry(activ, []) ->
    create_entry(activ, false);
create_entry(activ, false) ->
    {activ, false};
create_entry(activ, true) ->
    {activ, false};

create_entry(timer, []) ->
    {timer, 0};
create_entry(timer, 0) ->
    {timer, 0};
create_entry(timer, Value) when is_list(Value) ->
    create_entry(timer, list_to_integer(Value));
create_entry(timer, Value) when is_integer(Value) ->
    {timer, Value};

create_entry(database, []) ->
    {database, []};
create_entry(database, Value) ->
    {database, Value};

create_entry(description, []) ->
    {description, "Please, add the description here"};   
create_entry(description, Value) ->
    {description, Value}.    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_description_test() ->
    ?assertEqual([{description, []}], create_entry(description, [])).

create_thing_test() ->
    node_config:start(),
    create_thing("test", [{actor, sensor}, {driver, {test_driver, call_sensor},[]}]).
-endif.
    