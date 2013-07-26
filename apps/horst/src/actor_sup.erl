%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(actor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([get_actors_pids/0, get_actors/0]).

-include("../include/horst.hrl").
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Config = config_handler:get_config(horst, ?CONFIG_FILE),    
    {ok, {{one_for_one, 1, 10000},config_handler:create_child_specs(actor, Config)}}.

get_actors_pids() ->
	List = supervisor:which_children(?MODULE),
	get_actors_pids(List, []).

get_actors_pids([], Acc) ->
	Acc;
get_actors_pids([{_Id, Pid, _Type, _Module}|T], Acc) ->
	get_actors_pids(T, [Pid|Acc]).

get_actors() ->
	supervisor:which_children(?MODULE).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.