%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(account_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link([Application]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Application]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Application]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(account, worker, Application)]}}.