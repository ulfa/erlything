%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(mail_client_driver).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1]).
-export([handle_msg/3]).

init(Config) ->
	lager:info("mail_client_driver:init('~p')", [Config]),	
	application:start(gen_smtpc).

handle_msg([Node ,Sensor, Id, Time, "RISING"], Config, Module_config) ->
	{options, Options} =proplists:get_value(options, Module_config),
	{sender, Sender} =proplists:get_value(sender, Module_config),
	{password, Password} = proplists:get_value(password, Module_config),
	{to, To} = proplists:get_value(to, Module_config),
	{subject, Subject} = proplists:get_value(subject, Module_config),
	gen_smtpc:send({Sender, Password}, To, Subject, "Motion detected", Options),
	lager:info("send mail notification"),
	Config;
handle_msg([Node ,Sensor, Id, Time, Body], Config, Module_config) ->
	lager:warning("mail_client_driver got the wrong message : ~p", [[Node ,Sensor, Id, Time, Body]]),
	Config.

stop(Config) ->
    application:stop(gen_smtpc),
    application:unload(gen_smtpc).  
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
