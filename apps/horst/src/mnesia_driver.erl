%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(mnesia_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
-include_lib("stdlib/include/qlc.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1, handle_msg/3]).
-export([select/5, select_entries/1]).
-export([table_exists/1, create_table_name/3, get_tables/0]).

init(Config) ->
	lager:info("~p:init('~p')", [?MODULE, Config]),
	{ok, Config}.

stop(Config) ->
	lager:info("~p:stop('~p')", [?MODULE, Config]),
	{ok, Config}.

handle_msg([Node ,Module, Id, Time, Body], Config, Module_config) ->
	handle_intern([Node ,Module, Id, Time, Body], Config, Module_config),
	Config.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
handle_intern([Node ,<<"system">> = Module, Id, Time, {info, {"System is started!",[]}} = Body], Config, Module_config) ->		
	case Node =:= atom_to_binary(node(), utf8) of   	
		true -> save_or_create(Node, Module, Id, Time, Body),				
				handle_saved_funrunner_messages(Config);
		false -> lager:info("we do nothing, because it is a message from a different node"), false
	end;
handle_intern([Node ,Module, Id, Time, Body], Config, Module_config) ->
	save_or_create(Node ,Module, Id, Time, Body).

save_or_create(Node ,Module, Id, Time, Body) ->
	Table_name = create_table_name(Node, Module, Id),
	case table_exists(Table_name) of
		false -> create_table(Table_name, [{attributes, [time, body]}]);
		true -> ok 
	end,
	save_values(Table_name, Time, Body).
	
select(Node, Module, Id, From_time, To_time) ->
	{atomic, Result} = mnesia:transaction(
		fun() ->
    		qlc:e(qlc:q([{Table, Time, Payload} || {Table, Time, Payload} <- mnesia:table(create_table_name(Node, Module, Id)), Time >= From_time, Time =< To_time]))
    		
		end),
	Result.

select_entries(Table) ->
	{atomic, Result} = mnesia:transaction(
		fun() ->
    		qlc:e(qlc:sort(qlc:e(mnesia:table(Table)), [{order, ascending}]))
		end),
	Result.

handle_saved_funrunner_messages(Config) ->	
	Messages = select_entries(create_table_name(node(),funrunner_driver,default)),
	[?SEND(Body) ||  {Node, Time, {save_result, Body}} <- Messages].	

create_table(Table_name, Record) ->
	{atomic, ok} = mnesia:create_table(Table_name, [{disc_only_copies, [node()]}|Record]). 

-spec table_exists(atom()) -> true | false.

table_exists(Table_name) ->
   Tables = mnesia:system_info(tables),
   lists:member(Table_name,Tables).

get_tables() ->
	{node(), mnesia:system_info(tables)}.

save_values(Table_name, Time, Body) ->
	mnesia:dirty_write({Table_name, binary_to_int(Time), Body}).

create_table_name(Node, Module, Id) when is_atom(Node), is_atom(Module), is_atom(Id) ->
	create_table_name(atom_to_binary(Node, utf8), atom_to_binary(Module, utf8), atom_to_binary(Id, utf8));

create_table_name(Node, Module, Id) when is_list(Node), is_list(Module), is_list(Id) ->
	create_table_name(list_to_binary(Node), list_to_binary(Module), list_to_binary(Id));

create_table_name(Node, Module, Id) ->
	list_to_atom(lists:flatten([binary_to_list(Node), ":", binary_to_list(Module), ":",  binary_to_list(Id)])). 

binary_to_int(Bin) ->
	list_to_integer(binary_to_list(Bin)).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_table_name_test() ->
	?assertEqual('test@node:module:id', create_table_name(<<"test@node">>, <<"module">>, <<"id">>)).

binary_to_int_test() ->
	?assertEqual(10, binary_to_int(<<"10">>)).
-endif.
