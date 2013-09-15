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
	{init, Flag, [{options,Options}, {sender, Sender}, {password, Password}, {to, To}, {subject, Subject}]} = lists:keyfind(init, 1, Module_config),
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
