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
-export([get_sensors/0]).

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
	[].

get_actors() ->
	[].

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.