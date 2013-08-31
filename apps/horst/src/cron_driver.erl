%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(cron_driver).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, handle_msg/3]).
-export([send_message/1]).

init(Config) ->
	lager:info("cron_driver:init('~p')", [Config]),	
	application:start(erlcron),
	Crontab = proplists:get_value(crontab, Config, []), 
	start_jobs(Crontab).

handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	lager:warning("cron_driver can't handle any message : ~p", [[Node ,Sensor, Id, Time, Body]]),
	Config.

send_message(Message) ->
	lager:info("cron_driver will send the following message : ~p", [Message]),
	Msg = sensor:create_message(node(), ?MODULE, Message) ,
	sensor:send_message(nodes(), Msg). 
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
start_jobs(Crontab) ->
	[erlcron:cron(Job) || Job <- Crontab].

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
