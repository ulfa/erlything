%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENS,E for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%% @doc API to start and stop the application and to interact with

-module(horst).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").

%% Application callbacks
-export([start/0, stop/0]).
-export([set_debug/0, set_info/0]).
-export([get_things/0, get_things/1]).
-export([get_pic/1, get_log/1]).
-export([set_active/2, get_state/1, set_state/2]).

start() ->
    %% i don't check the return value, because i also run it on a pc 
 	application:start(gpio),    
    ensure_started(?MODULE),    
    ?SEND(?SYSTEM, {info, {"horst is started!",[]}}). 

stop() ->
    ?SEND(?SYSTEM, {info, {"horst is going down!",[]}}),
    ensure_stopped(gpio), 
    ensure_stopped(horst).

get_things() ->
    {node(), get_things(things_sup:get_things(), [])}.
get_things(sensor) ->
    {node(), get_things(things_sup:get_sensors(), [])};
get_things(actor) ->
    {node(), get_things(things_sup:get_actors(), [])}.
get_things([], Acc) ->
	Acc;
get_things([{Name, Pid, _X, _Y}|Things], Acc) ->
	{{Driver, _Func}, _Config} = thing:get_driver(Name),
	get_things(Things, [{Pid, Name, thing:get_start_time(Name), thing:get_description(Name), Driver, thing:get_icon(Name)}|Acc]).

get_pic(Name) ->
    file_provider_sup:get_pic(Name). 
get_log(Name) ->
    file_provider_sup:get_log(Name). 

set_debug() ->
	lager:set_loglevel(lager_console_backend, debug).

set_info() ->
	lager:set_loglevel(lager_console_backend, info).

set_active(Thing, Status) ->
    node_config:set_active(Thing, Status).

get_state(Thing) ->
    thing:get_state(Thing).

set_state(Thing, State) ->
    thing:set_state(Thing, State). 

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

ensure_stopped(App) ->
    case application:stop(App) of
        ok ->
            ok;
        {error, _Reason} ->
            ok
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.