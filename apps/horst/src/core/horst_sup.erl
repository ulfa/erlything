%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(horst_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARG(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).
-define(CHILD_ARG_1(I, Type, Arg1), {Arg1, {I, start_link, [Arg1]}, temporary, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [                                  
    							  ?CHILD(actor_group, worker),
    							  ?CHILD(node_config, worker),    							
    							  ?CHILD(things_sup,supervisor),
    							  ?CHILD_ARG_1(thing_event, worker, "thing_event_manager"),
                                  ?CHILD(file_provider_sup,supervisor)    							  
    							  ]}}.