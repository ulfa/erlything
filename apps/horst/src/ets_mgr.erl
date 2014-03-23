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
-module(ets_mgr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([init_table/3, backup/1, restore/1, delete/1]).

%% ====================================================================
%% External functions
%% ====================================================================
init_table(Pid, Name, Data) ->
	gen_server:call(?MODULE, {init_table, Pid, Name, Data}).
backup(Table) ->
    gen_server:call(?MODULE, {backup_table, Table}).
restore(Table) ->
    gen_server:call(?MODULE, {restore_table, Table}).
delete(Table) ->
    gen_server:call(?MODULE, {delete_table, Table}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {}).
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
	process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call({init_table, Pid, Name, Data}, From, State) ->
	lager:info("init_table for name : ~p ", [Name]),
	link(Pid),
	Table_Name = case ets:info(Name) of 
		undefined ->
			lager:info("create new ets table ~p", [Name]),
		    TableId = ets:new(Name, [public, named_table]),
    		ets:insert(TableId, Data),
    		ets:setopts(TableId, {heir, self(), Data}),
    		ets:give_away(TableId, Pid, Data),
    		TableId;
    	InfoList ->
    		lager:info("use existing ets table : ~p", [Name]),
    		ets:setopts(Name, {heir, self(), Data}),
    		ets:give_away(Name, Pid, Data),	 
    		Name
    end,
    {reply, Table_Name, State};

handle_call({backup_table, Table}, From, State) ->
    Path = application:get_env(horst, backup_path),
    ok = check_path(Path),
    Reply = ets:tab2file(filename:join(filename:absname(Path), Table)),
    {reply, Reply, State};

handle_call({restore_table, Table}, From, State) ->
    Path = application:get_env(horst, backup_path),
    ok = check_path(Path),
    Reply = ets:file2tab(Table, filename:join(filename:absname(Path) , [{verify,true}])), 
    {reply, Reply, State};

handle_call({delete_table, Table}, From, State) ->
    ets:delete(Table),
    {reply, ok, State};

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
handle_info({'EXIT',Pid, killed}, State) ->
    lager:info("Thing (~p) !! is now dead, farewell TableId: ~n", [Pid]),
    {noreply, State};

handle_info({'EXIT',Pid, Reason}, State) ->
	lager:info("Exit Pid : ~p with reason :~p ", [Pid, Reason]),	
	{noreply, State};

handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
	lager:info("Thing_pid : ~p",[Pid]),
    lager:info("Warning TableId: ~p OwnerPid: ~p is dying", [TableId, Pid]),
    lager:info("Thing(~p) => ETS Manager(~p) handling TableId: ~p~n", [Pid, self(), TableId]),
    {noreply, State};

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
check_path(Path) ->
    case filelib:is_dir(Path) of 
        false -> file:make_dir(Path);
        true -> ok
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.