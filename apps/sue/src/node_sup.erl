%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(node_sup).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start_child/1]).
-export([init/1]).

-export([get_children/0, is_child/1]).

-define(LHS(),{node, {node, start_link, []}, temporary, brutal_kill, worker, [node]}).
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child([Node, Ip, Uptime]) when is_binary(Node) ->
	start_child([erlang:binary_to_atom(Node, latin1), Ip, Uptime]);

start_child([Node, Ip, Uptime]) ->
	supervisor:start_child(?MODULE, [Node, Ip, Uptime]).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->		
	RestartStrategy = {simple_one_for_one, 1, 3600},
    {ok, {RestartStrategy, [?LHS()]}}.

get_children() ->
	List = supervisor:which_children(?MODULE),
	get_children(List, []).

get_children([], Acc) ->
	Acc;
get_children([{_Id, Node, _Type, _Module}|T], Acc) ->
	get_children(T, [node:get_status(Node)|Acc]).	

is_child(Node) ->
	case lists:keyfind(Node, 1, get_children()) of
		false -> false;
		_ -> true
	end.