%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(file_mover_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, call_sensor/2]).

init(Config) ->
    Module_config = config:get_module_config(Config),
    Destination = config:get_value(destination, Module_config), 
    mount(Destination),
    {ok, Config}.

call_sensor(Config, Module_config) ->
    [Filter, Source, Destination] = config:get_level_values([data], [file_format, source, destination], Module_config), 
    move_files(Destination, find_files(Source, Filter)),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
move_files(Destination, []) ->
    ok;
move_files(Destination, [File|List_of_files]) ->
    lager:info("move file : ~p to dest : ~p", [File, Destination]),
    Basefile = filename:basename(File),
    {ok, BytesCopied} = file:copy(File, filename:join(Destination, Basefile)),
    ok = file:delete(File), 
    sensor:send(?MODULE, [{moved_file, Basefile}]), 
    move_files(Destination, List_of_files).

find_files(Path, Filter) ->
    filelib:wildcard(filename:join(Path, Filter)). 

mount(Destination) ->
    os:cmd("mount -a -o nolock").

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
