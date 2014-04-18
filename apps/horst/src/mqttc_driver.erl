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
    case emqttc:start_link([{host, "broker.mqttdashboard.com"}]) of 
        {ok, Pid} -> Pid;
        {error, Reason} -> sensor:send('system', {error, {"can't start mqttc_driver", Reason}})
    end,
    {ok, Config}.
    
stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    application:stop(emqttc),
    {ok, Config}.

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
    lager:info("~p got a message with body : ~p : ", [?MODULE, Body]),
    Topic = create_topic("erlyThing", Node, Sensor, Id),
    emqttc:publish(emqttc, Topic, term_to_binary(Body)),
    Config.

create_topic(Prefix, Node ,Sensor, Id) ->
    filename:join([Prefix, Node, Sensor, Id]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_topic_test() ->
    ?assertEqual(<<"erlyThing/test/test1/default">>, create_topic("erlyThing", "test", "test1", "default")).
-endif.
