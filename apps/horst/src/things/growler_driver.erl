%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(growler_driver).

%% --------------------------------------------------------------------
%% defines
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, handle_msg/3]).

init(Config) ->
    growler_sup:start_link(),
    {ok, Config}.

handle_msg([Node ,<<"system">>, Id, Time, Optional, {error,{dead, Node1}}], Config, Module_config) -> 
    send_message(error, lists:flatten(io_lib:format("Node: ~p is dead!", [Node1]))),
    Config;   

handle_msg([Node ,<<"system">>, Id, Time, Optional, {info, {alive, Node1}}], Config, Module_config) -> 
    send_message(info, lists:flatten(io_lib:format("Node: ~p is alive!", [Node1]))),
    Config;   

handle_msg([Node ,Sensor, Id, Time, Optional, {error,{Config_file, Text, Reason}}], Config, Module_config) -> 
    send_message(error, lists:flatten(io_lib:format("Config: ~p is corrupt", [Config_file]))),
    Config;   


handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got a message with incorrect values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
send_message(error, Message) ->
    lager:info("Message: ~p", [Message]),
    growler:error(Message);
send_message(info, Message) ->
    lager:info("Message: ~p", [Message]),
    growler:success(Message). 

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
