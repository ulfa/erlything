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
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1, handle_msg/3]).

init(Config) ->
    lager:info("~p:init('~p')", [?MODULE, Config]),
    MC = config:get_module_config(Config),  
    [Host, Port] = config:get_values([host, port], MC),
    %%case emqttc:start_link([{host, Host}, {port, Port}]) of 
    case emqttc:start_link([]) of 
        {ok, Pid} -> Pid;
        {error, Reason} -> sensor:send('system', {error, {"can't start mqttc_driver", Reason}})
    end,
    subscribe(),
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

create_message(Topic, {Time, Body}) ->
    [Prefix, Node, Sensor, Id] = binary:split(Topic, <<"/">>, [global]),
    [Node, Sensor, Id, Time, Body].

subscribe() ->
    emqttc:subscribe(emqttc, [{<<"erlyThing/horst@Anjas-MacBook-Air/sample_driver/default">>, 0}]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_topic_test() ->
    ?assertEqual(<<"erlyThing/test/test1/default">>, create_topic("erlyThing", "test", "test1", "default")).

create_message_test() ->
    T = <<"erlyThing/test@node/sensor/id">>,
    Payload = {<<1234567>>, {"sample", test}},
    ?assertEqual([<<"test@node">>, <<"sensor">>, <<"id">>, Payload], create_message(T, Payload)).
-endif.
