%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(sensortag_driver).

%% --------------------------------------------------------------------
%% defines
%% --------------------------------------------------------------------
-define(MAC, "34:B1:F7:D5:58:5B").
-define(GATTTOOL(Mac),io_lib:format("gatttool -b ~p --interactive", [Mac])).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,start1/0]).
-export([init/1, stop/1, handle_msg/3]).

init(Config) -> 
    Port = open_port({spawn, "gatttool -b 34:B1:F7:D5:58:5B --interactive"}, [binary, eof]),
    Config_1 = config:set_value_data(port, Port, Config),
    {ok, Config_1}.

stop(Config) ->
    {ok, Config}.

%%<<"[   ][34:B1:F7:D5:58:5B][LE]> ">>
handle_msg({data, <<"[   ][34:B1:F7:D5:58:5B][LE]> ">>} = Payload, Config, Module_config) ->
    lager:info("1... payload : ~p", [Payload]),
    [Port] = config:get_level_values([data],[port], Module_config),
    lager:info("...... Port : ~p", [Port]),
    port_command(Port, "connect\r") ,
    Config;

handle_msg({data, <<"\r\e[16@[CON][34:B1:F7:D5:58:5B][LE]>\e[C">>} = Payload, Config, Module_config) ->
    lager:info("2... payload : ~p", [Payload]),
    [Port] = config:get_level_values([data],[port], Module_config),
    port_command(Port, "char-read-hnd 0x25\r"),
    Config;

handle_msg({data, <<"\nCharacteristic value/descriptor: 00 00 00 00 \n[CON][34:B1:F7:D5:58:5B][LE]> ">>} = Payload, Config, Module_config) ->
    lager:info("3... payload : ~p", [Payload]),
    [Port] = config:get_level_values([data],[port], Module_config),
    port_command(Port, "char-write-cmd 0x29 01\r"),
    port_command(Port, "char-read-hnd 0x25\r"),
    Config;

handle_msg({data, <<"\nCharacteristic value/descriptor: a9 ff b0 0a \n[CON][34:B1:F7:D5:58:5B][LE]> ">>} = Payload, Config, Module_config) ->
    lager:info("3... payload : ~p", [Payload]),
    Config;

handle_msg({Port, eof}, Config, Module_config) ->
    lager:info("1... Port : ~p", [Port]),
    Config;

handle_msg({data, Payload}, Config, Module_config) ->
    lager:info("any payload : ~p", [Payload]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
start()->
    spawn(?MODULE, start1,[]).

start1() ->
    Port = open_port({spawn, "gatttool -b 34:B1:F7:D5:58:5B --interactive"}, [binary, eof]),    
    read_port(Port).

read_port(Port) ->

 receive
    {Port,{data,<<"[   ][34:B1:F7:D5:58:5B][LE]> ">>}} ->  
        io:format("1...."),
        Port ! {self(), 'connect'};       
    {Port,{data, Data}} ->
        io:format("Data: ~p~n",[Data]);     
    Any ->
      io:format("irgendwas, ~p~n",[Any])
  end,
    read_port(Port).
     
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
