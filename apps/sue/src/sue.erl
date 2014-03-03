%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

%% @doc API to start and stop the application and to interact with

-module(sue).

%% Application callbacks
-export([start/0, stop/0]).
-export([get_children/0, get_children/1, add_node/1, sys_info/1, etop/1, memory/1]).
-export([get_applications/1, process_info/2, app_info/2]).
-export([set_debug/0, set_info/0]).

%% doc starrt the application
%%
	start() ->
		%%application:start(lager),		
	  	application:start(?MODULE).

%% doc stop the application
	stop() ->
		%%application:stop(lager),		
		application:stop(?MODULE).

%% doc Return all registered children. 
%% 
%% A child is Node which was registered throw the transceiver module
	get_children() ->
		node_sup:get_children().

%% doc Return the node with the name node
-spec get_children(atom()) -> node().

	get_children(Node) ->
		case rpc:call(Node, node_sup, get_children, []) of
			{badrpc,nodedown} -> [];
			Any -> Any
		end.
%% doc adds a node to the list of the nodes
-spec add_node(atom()) -> {ok, Child :: pid()}.			
	add_node(Node) when is_atom(Node) ->
		node_sup:start_child([Node, {0,0,0,0}]).
	
%% doc returns the sys_info of an node
-spec sys_info(atom()) -> [{atom(), any()}].
	sys_info(Node) ->
		lists:keysort(1,node:sys_info(Node)).
	
	etop(Node) ->
		node:etop(Node).

	process_info(Node, Pid) ->
		node:pid_info(Node, Pid).

	memory(Node) ->
		node:memory(Node).

	get_applications(Node) when is_atom(Node)->
		node:get_applications(Node). 

	app_info(Node, App) ->
		process_info:start(),
		[].

	set_debug() ->
		lager:set_loglevel(lager_console_backend, debug).

	set_info() ->
		lager:set_loglevel(lager_console_backend, info).	