%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(funrunner).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0,start_link/0]).
-export([save/3, run/2, list_all/0, list/1, run_command/2]).
-export([test_init/0]).
%% ====================================================================
%% External functions
%% ====================================================================
save(Name, Command, Comment) ->
	gen_server:call(?MODULE, {save, Name, Command, Comment}).
run(Name, Args) ->
	gen_server:call(?MODULE, {run, Name, Args}).
run_command(Command, Args) ->
	gen_server:call(?MODULE, {run_command, Command, Args}).
list(Name) ->
	gen_server:call(?MODULE, {list, Name}).
list_all() ->
	gen_server:call(?MODULE, {list_all}).

test_init() ->
	funrunner:save("arg_test", "fun(X) -> X + 1 end.", "Das ist ein Argument Test").
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {funs}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{funs=[]}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({save, Name, Command, Comment}, From, State=#state{funs=Funs}) ->
	Fun = command_to_fun(Command),
    {reply, ok, State#state{funs=[{Name, Fun, Command, Comment}|Funs]}};	
handle_call({run, Name, Args}, From, State=#state{funs=Funs}) ->	
	Reply = case get_fun(Funs, Name) of
		[] -> lager:error("no fun found for name : ~p~n", [Name]);
		F -> run_fun(F, Args)
	end,
    {reply, Reply, State};
handle_call({run_command, Command, Args}, From, State) ->
	Fun = command_to_fun(Command),
	Reply = run_fun(Fun, Args),
	{reply, Reply, State};
handle_call({list, Name}, From, State=#state{funs=Funs}) ->	
    {reply, get_command(Funs, Name), State};
handle_call({list_all}, From, State=#state{funs=Funs}) ->	
    {reply, {node(), get_commands(Funs)}, State};	
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
command_to_fun(Command) ->
	{ok, Tokens,_} = erl_scan:string(Command),	
	{ok, Fun} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:exprs(Fun, []),
	Value.
run_fun(Fun, Args) when is_function(Fun) ->
	Fun(Args).		
get_fun(Funs, Name) ->	
	case lists:keysearch(Name, 1, Funs) of
		{value, {N, F, C, Co}} -> F;
		false -> []
	end.
get_commands(Funs) ->
	[{N, C, Co} || {N, F, C, Co} <- Funs].	
get_command(Funs, Name) ->
	case lists:keysearch(Name, 1, Funs) of
		{value, {N, F, C, Co}} -> {N, C, Co};
		false -> []
	end.
	
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_fun_test() ->
	A = [{"test", fun([])-> "testme" end, "testme", "Das ist Test1"}, {"test1", fun([])-> "testme1" end, "testme1", "Das ist Test1"}],
	?assertEqual(true, erlang:is_function(get_fun(A, "test"))).
get_command_test() ->
	A = [{"test", fun([])-> "testme" end, "testme", "Das ist Test1"}, {"test1", fun([])-> "testme1" end, "testme1", "Das ist Test1"}],
	?assertEqual({"testme1", "Das ist Test1"}, get_command(A, "test1")).
get_commands_test() ->	
	A = [{"test", fun([])-> "testme" end, "testme", "Das ist Test1"}, {"test1", fun([])-> "testme1" end, "testme1", "Das ist Test1"}],
	?assertEqual(2, length(get_commands(A))).
command_to_fun_test() ->
	?assertEqual(true, erlang:is_function(command_to_fun("fun([]) -> \"test\" end."))),	
	?assertEqual(true,erlang:is_function(command_to_fun("fun([]) -> erlang:system_info(info) end."))).
run_fun_test() ->
	F = command_to_fun("fun([]) -> \"test\" end."),
	?assertEqual("test",run_fun(F, [])).
inject_test() ->
	funrunner:start(),
	?assertEqual(ok, funrunner:save("test", "fun([]) -> \"test\" end.", "Das ist ein Test")).
list_test() ->
	funrunner:start(),
	funrunner:save("test", "fun([]) -> \"test\" end.", "Das ist ein Test"),
	?assertEqual({"fun([]) -> \"test\" end.", "Das ist ein Test"}, funrunner:list("test")).
run_test() ->
	funrunner:start(),
	funrunner:save("test", "fun([]) -> \"test\" end.", "Das ist ein Test"),
	?assertEqual("test", funrunner:run("test", [])).
run_commmand_test() ->
	funrunner:start(),
	?assertEqual("test", funrunner:run_command("fun([]) -> \"test\" end.", [])).
run_commmand_with_arg_test() ->
	funrunner:start(),
	funrunner:save("arg_test", "fun(X) -> X + 1 end.", "Das ist ein Argument Test"),
	?assertEqual(2, funrunner:run("arg_test", 1)).
-endif.