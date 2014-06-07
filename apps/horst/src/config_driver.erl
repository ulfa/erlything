%% Copyright 2014 Ulf Angermann
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
%%% -------------------------------------------------------------------
-module(config_driver).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1, handle_msg/3]).

init(Config) ->
    Config.

stop(Config) ->
    Config.

handle_msg([Node ,Sensor, Id, Time, [{action, "copy"}, {thing, Thing}, {target, Target}]] = Msg, Config, Module_config) ->
    lager:info("~p got message : ~p", [?MODULE, Msg]),
    Thing_config = node_config:get_thing_config(Thing),    
    case rpc:call(list_to_atom(Target), node_config, add_thing_to_config, [Thing_config, ?THINGS_CONFIG]) of 
        Result -> lager:info("copied ~p - config to node : ~p !", [Thing, Target]),
                  ?SEND(lists:flatten(io_lib:format("copied ~p - config to node : ~p !", [Thing, Target])));
        {badrpc, Reason} -> lager:error("couldn't copy ~p - config to node : ~p", [Thing, Thing_config]),
                            ?SEND(lists:flatten(io_lib:format("couldn't copy ~p - config to node : ~p", [Thing, Thing_config])))
    end,
    Config;

handle_msg([Node ,Sensor, Id, Time, [{action, "delete"}, {thing, Thing}, {target, Target}]] = Msg, Config, Module_config) ->
    lager:info("~p got message : ~p", [?MODULE, Msg]),
    Thing_config = node_config:get_thing_config(Thing),
    case rpc:call(list_to_atom(Target), node_config, delete_thing_config, [Thing_config, ?THINGS_CONFIG]) of 
        Result -> lager:info("deleted ~p - config from node : ~p !", [Thing, Target]),
                  ?SEND(lists:flatten(io_lib:format("deleted ~p - config from node : ~p !", [Thing, Target])));
        {badrpc, Reason} -> lager:error("couldn't deleted ~p - config from node : ~p !", [Thing, Target]),
                            ?SEND(lists:flatten(io_lib:format("couldn't cdeleted ~p - config from node : ~p !", [Thing, Target])))
    end,
    Config;

handle_msg([Node ,Sensor, Id, Time, [{action, "export"}, {thing, Thing}]] = Msg, Config, Module_config) ->
    lager:info("~p got message : ~p", [?MODULE, Msg]),
    Thing_config = node_config:get_thing_config(Thing),
    ?SEND(lists:flatten(io_lib:format("export ~p - config from node.", [Thing]))),
    Config;

handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got the wrong message : ~p", [?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
