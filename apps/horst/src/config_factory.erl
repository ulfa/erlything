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
-export([check_thing/1]).

create_thing(Name, Config) ->
    Existing_config = get_config(),
    Thing_config = case is_entry(Name,  Existing_config) of 
        true -> lager:error("Entry with name : ~p already exists!", [config:get_value(thing, Existing_config)]),
                [];
        false -> create_entries(Name, Config)
    end,
    {thing, Name, Thing_config}.
    
check_thing({thing, Name, Parameters}) ->
    [is_valid(Key, config:get_value(Key,Parameters)) || Key <- [type, ets, icon, id, driver, activ, timer, database, description]].
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
is_valid(Key, Parameter) ->
    {Key, Parameter} = create_entry(Key, Parameter).
    
create_entries(Name, Config) ->
    [create_entry(Key, config:get_value(Key, Config)) || Key <- [type, ets, icon, id,driver, activ, timer, database, description]].
    
get_config() ->
    {Node, Config} = node_config:get_things_config(),
    Config.

is_entry(Name, Config) ->
    lists:keymember(Name, 2, Config).     

create_entry(type, sensor) ->
    {type, sensor};    

create_entry(type, actor) ->
    {type, actor};
create_entry(type, sensor) ->
    {type, sensor};    
    
create_entry(id, undefined) ->
    create_entry(id, "default");
create_entry(id, Value) ->
    {id, Value};

create_entry(icon, []) ->
    create_entry(icon, "thing.png");
create_entry(icon, Value) ->
    {icon, Value};

create_entry(ets, undefined) ->
    create_entry(ets, false);
create_entry(ets, true) ->
    {ets, true};
create_entry(ets, false) ->
    {ets, false};

create_entry(activ, undefined) ->
    create_entry(activ, false);
create_entry(activ, false) ->
    {activ, false};
create_entry(activ, true) ->
    {activ, false};

create_entry(driver, {Driver, Function, Parameters}) when  is_atom(Driver), is_atom(Function), is_list(Parameters)->
    {driver, {Driver, Function}, Parameters};

create_entry(timer, undefined) ->
    {timer, 0};
create_entry(timer, 0) ->
    {timer, 0};
create_entry(timer, Value) when is_list(Value) ->
    create_entry(timer, list_to_integer(Value));
create_entry(timer, Value) when is_integer(Value) ->
    {timer, Value};

create_entry(database, undefined) ->
    {database, []};
create_entry(database, Value) ->
    {database, Value};

create_entry(description, undefined) ->
    {description, "Please, add the description here"};   
create_entry(description, Value) ->
    {description, Value}.    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_description_test() ->
    ?assertEqual({description, "Please, add the description here"}, create_entry(description, undefined)).

create_config_test() ->
    application:load(horst),
    node_config:start(),
    Result = {thing,"Sample_Sensor1",
     [{type,sensor},
      {ets,true},
      {icon,"temp.png"},
      {id, "default"},
      {driver,{sample_driver,call_sensor},[{init,true},{data,[]}]},
      {activ,false},
      {timer,5000},
      {database,[]},
      {description,"Sample sensor for playing with"}]},
      ?assertEqual(Result, create_thing("Sample_Sensor1",[{type, sensor}, {ets, true}, {icon, "temp.png"}, 
        {driver, {sample_driver, call_sensor, [{init, true}, {data, []}]}}, {timer, 5000}, {description,"Sample sensor for playing with"}])). 
-endif.
    