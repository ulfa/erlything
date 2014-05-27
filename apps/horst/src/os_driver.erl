%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(os_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1]).
-export([call_sensor/2]).


init(Config) ->
    lager:info("~p:init('~p')", [?MODULE, Config]),
    application:start(os_mon),
    {ok, Config}.

stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    lager:info("stop is called"),
    application:stop(os_mon),
    application:unload(os_mon), 
    {ok, Config}.    

call_sensor(Config, Module_config) ->
    Data = call_driver(),    
    ?SEND(Data),
    %%thing:save_data_to_ets(Config, Data),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
call_driver() ->
    [{temp, read_temp()}, {avg1, divide(cpu_sup:avg1())}, {avg5, divide(cpu_sup:avg5())}, {avg15, divide(cpu_sup:avg15())}].

read_temp() ->
    {ok, T} = file:read_file("/sys/class/thermal/thermal_zone0/temp"),
    Temp = erlang:list_to_integer(string:strip(binary_to_list(T), right, $\n)) / 1000.

divide(Value) ->
    Value/256.
    
%%" 17:56:58 up  6:34,  3 users,  load average: 0.76, 0.78,"
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
