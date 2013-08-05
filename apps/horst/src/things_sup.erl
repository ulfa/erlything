%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(things_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([get_sensors/0, get_actors/0, get_sensors_pids/0, get_actors_pids/0]).

-include("../include/horst.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	Config = config_handler:get_config(horst, ?THINGS_CONFIG),
    {ok, {{one_for_one, 1, 10000}, config_handler:create_thing_spec(Config)}}.

get_sensors() ->
	[{Name, Pid, Type, Modules} || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_sensor(thing:get_type(Name))].

get_actors() ->
	[{Name, Pid, Type, Modules}|| {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_actor(thing:get_type(Name))].

get_sensors_pids() ->
	[Pid || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_sensor(thing:get_type(Name))].

get_actors_pids() ->
	[Pid || {Name, Pid, Type, Modules} <- supervisor:which_children(things_sup), is_actor(thing:get_type(Name))].

is_actor(actor) ->
	true;
is_actor(sensor) ->
	false.

is_sensor(sensor) ->
	true;
is_sensor(actor) ->
	false.
	
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.