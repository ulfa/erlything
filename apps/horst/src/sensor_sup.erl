%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(sensor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([get_sensors/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_1(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).
-define(CONFIG_FILE, "sensor.config").
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Module) ->
	io:format("Modul : ~p~n", [?CHILD(Module, worker)]),
    supervisor:start_child(Module, ?CHILD(Module, worker)).

create_child_specs() ->
	Config = get_config(),
	[?CHILD(Sensor, worker) || {Sensor} <- Config].

init([]) ->
    {ok, {{one_for_one, 1, 10000}, create_child_specs()}}.

get_sensors() ->
	supervisor:which_children(?MODULE).

get_config() ->
	{ok, Config} = file:consult(filename:join([code:priv_dir(horst),"config", ?CONFIG_FILE])),
	Config.
	
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

get_config_test() ->
	application:load(horst),
	Config = get_config(),
	%%	?debugFmt("Config File : ~p", [Config]),
	?assertEqual([{hc_sr501_sensor}, {dht22_sensor}], Config).


start_child_test() ->
	application:load(horst),
	?assertEqual([?CHILD(hc_sr501_sensor, worker), ?CHILD(dht22_sensor, worker)], create_child_specs()).
-endif.