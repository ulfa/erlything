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
-export([init/1, stop/1, handle_msg/3]).
-export([send_message/1]).

init(Config) ->
	lager:info("cron_driver:init('~p')", [Config]),	
	Crontab = proplists:get_value(crontab, Config, []), 
    case application:start(erlcron) of
        ok -> start_jobs(Crontab),
        	  ok;
        {error, {already_started, erlcron}} ->
            ok
    end,
    {ok, Config}.
    
stop(Config) ->
	lager:info("stopping Module : ~p ", [?MODULE]),
	Crontab = proplists:get_value(crontab, Config, []), 
	stop_jobs(Crontab),
	application:stop(erlcron),
    {ok, Config}.

handle_msg([Node ,Sensor, Id, Time, {once, {Hour, Minutes, pm}}], Config, Module_config) ->
    Config;	 

handle_msg([Node ,Sensor, Id, Time, {once, Seconds}], Config, Module_config) ->
    Config;  

handle_msg([Node ,Sensor, Id, Time, {{daily, {every, {Seconds, sec}, {between, {From_hour, pm}, {To_hour, To_minutes, pm}}}}}], Config, Module_config) ->
    Config;  

handle_msg([Node ,Sensor, Id, Time, {daily, {Hour, Minutes, pm}}], Config, Module_config) ->
    Config;  

handle_msg(Message, Config, Module_config) ->
    lager:warning("cron_driver can't handle any message"),
    Config.

send_message(Message) ->
	lager:info("cron_driver will send the following message : ~p", [Message]),
	sensor:send(?MODULE, Message). 
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
start_jobs(Crontab) ->
	[erlcron:cron(Job) || Job <- Crontab].

stop_jobs(Crontab) ->
	[erlcron:cancel(Job) || Job <- Crontab].


%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
