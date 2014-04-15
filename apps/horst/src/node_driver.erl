%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 04.11.2013
-module(node_driver).

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
    ok = tranceiver:register_listener(self()),
    {ok, Config}.
    

stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    ok = tranceiver:unregister_listener(self()),
    {ok, Config}.

handle_msg({external_interrupt, sue, info, {alive, Node}}, Config, Module_config) ->
    lager:info("~p got a message that node : ~p is ALIVE!", [?MODULE, Node]),
    sensor:send('system', {info, {alive, Node}}),
    Config;

handle_msg({external_interrupt, sue, error, {dead, Node}}, Config, Module_config) ->
    lager:info("~p got a message that node : ~p is DEAD!", [?MODULE, Node]),
    sensor:send('system',{error, {dead, Node}}),
    Config.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
