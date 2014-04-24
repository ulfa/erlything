%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 18.04.2014
-module(mqttc_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External 
%% --------------------------------------------------------------------

-export([init/1, stop/1, handle_msg/3]).

init(Config) ->
    lager:info("~p:init('~p')", [?MODULE, Config]),
    MC = config:get_module_config(Config),  
    [Host, Port, Filter] = config:get_values([host, port, filter], MC),    
    ok = application:start(emqttc),
    mqttc_event:start_link() ,
    case emqttc:start_link([{host, Host}, {port, Port}]) of 
        {ok,Pid} -> Pid;
        {error,{already_started,Pid}} -> Pid 
    end,
    emqttc_event:add_handler(mqttc_event, Filter),
    {ok, Config}.
    
stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    application:stop(emqttc),
    application:unload(emqttc),
    {ok, Config}.

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
    lager:info("~p got a message with body : ~p : ", [?MODULE, Body]),
    Prefix = proplists:get_value(prefix, Module_config, ?MQTT_PREFIX),    
    emqttc:publish(emqttc, create_topic(Prefix, Node, Sensor, Id), create_payload(Time, Body)),
    Config.

create_topic(Prefix, Node ,Sensor, Id) ->
    filename:join([Prefix, Node, Sensor, Id]).

create_payload(Time, Body) ->
    term_to_binary({binary_to_integer(Time), Body}).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_topic_test() ->
    ?assertEqual(<<"erlyThing/test/test1/default">>, create_topic("erlyThing", "test", "test1", "default")).
-endif.
