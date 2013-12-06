%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 29.11.2013

-module(house_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_room/1, start_device/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(ROOM(Id, Config), {Id, {room, start_link, [Config]}, transient, 5000, worker, [room]}).
-define(DEVICE(RF_address, Config), {RF_address, {device, start_link, [Config]}, transient, 5000, worker, [device]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_room(Config) ->
    supervisor:start_child(?MODULE, ?ROOM(proplists:get_value(room_id, Config), Config)).

start_device(Config) ->
    supervisor:start_child(?MODULE, ?DEVICE(proplists:get_value(rf_address, Config), Config)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(house, worker)]}}.