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
	application:start(gen_smtpc),
	{ok, Config}.


handle_msg([Node ,Sensor, Id, Time, Optional, [{to, To}, {subject, Subject}, {content, Content}]], Config, Module_config) ->
	Options = proplists:get_value(options, Module_config),
	Sender = proplists:get_value(sender, Module_config),
	Password = proplists:get_value(password, Module_config),
	send_email(Sender, Password, To, Subject, "Motion detected", Options),
	Config;

handle_msg([Node ,Sensor, Id, Time, Optional, "RISING"], Config, Module_config) ->
	Options = proplists:get_value(options, Module_config),
	Sender  = proplists:get_value(sender, Module_config),
	Password  = proplists:get_value(password, Module_config),
	To = proplists:get_value(to, Module_config),
	Subject = proplists:get_value(subject, Module_config),
	send_email(Sender, Password, To, Subject, "Motion detected", Options),
	Config;

handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, Module_config) ->
	lager:warning("mail_client_driver got the wrong message : ~p", [[Node ,Sensor, Id, Time, Body]]),
	Config.

send_email(Sender, Password, To, Subject, Content, Options) ->
	lager:debug("Sender : ~p Password : ~p To : ~p Subject : ~p Content : ~p Options : ~p", [Sender, Password, To, Subject, Content, Options]),
	gen_smtpc:send({Sender, Password}, To, Subject, Content, Options),
	lager:info("sending email").


stop(Config) ->
    application:stop(gen_smtpc),
    application:unload(gen_smtpc),
    {ok, Config}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
