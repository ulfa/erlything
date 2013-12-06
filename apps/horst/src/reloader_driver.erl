%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 14.11.2013
-module(reloader_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([call_sensor/2]).
-export([init/1, stop/1]).

init(Config) ->
    lager:info("~p:init('~p')", [?MODULE, Config]),
    application:start(reloader).

stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    application:stop(reloader),
    application:unload(reloader).  

call_sensor(Config, Module_config) ->   
    Changed_Modules = re_reloader:reload(),
    send_message(Config, Changed_Modules),
    Config.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
send_message(Config, []) ->
    lager:debug("there are no modules which i could reload.");

send_message(Config, Changed_Modules) ->
    Msg = sensor:create_message(node(), ?MODULE, sensor:get_id(Config), Changed_Modules), 
    sensor:send_message(Msg).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.