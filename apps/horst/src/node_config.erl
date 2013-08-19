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
-module(node_config).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================
-export([get_messages_config/0, get_things_config/0]).
-export([set_messages_config/2, set_things_config/2]).
-export([get_messages_for_module/1]).

get_messages_config() ->
	gen_server:call(?MODULE, {get_messages_config}).
get_things_config() ->
	gen_server:call(?MODULE, {get_things_config}).
get_messages_for_module(Module) ->
	gen_server:call(?MODULE, {get_messages_for_module, Module}).
set_messages_config(Key, Value) ->
	gen_server:call(?MODULE, {set_messages_config, Key, Value}).
set_things_config(Key, Value) ->
	gen_server:call(?MODULE, {set_things_config, Key, Value}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {things, messages, last_poll_datetime}).
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
    {ok, #state{things=[], messages=[], last_poll_datetime=get_poll_time()}, 0}.

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
handle_call({get_messages_config}, From, State=#state{messages=Messages}) -> 
    {reply, {node(), Messages}, State};

handle_call({get_things_config}, From, State=#state{things=Things}) ->
    {reply, {node(), Things}, State};

handle_call({get_messages_for_module, Module}, From, State=#state{messages=Messages}) ->
	Config = config_handler:get_messages_for_module(Messages, Module), 
    {reply, Config, State};

handle_call({set_messages_config, Key, Value}, From, State) ->
    {reply, "not implemented yet", State};

handle_call({set_things_config, Key, Value}, From, State) ->
    {reply, "not implemented yet", State};

handle_call(Request, From, State) ->
    {reply, "not implemented yet", State}.

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
handle_info(timeout, State) ->
    Things = config_handler:get_config(?APPLICATION, ?THINGS_CONFIG),
    Messages = config_handler:get_config(?APPLICATION, ?MESSAGES_CONFIG),
    start_timer(),
    {noreply, State#state{things=Things, messages=Messages}};

handle_info(update_config, State=#state{last_poll_datetime=Last_poll_datetime}) ->
    Things_mtime = get_mtime(?APPLICATION, ?THINGS_CONFIG),
    Messages_mtime = get_mtime(?APPLICATION, ?MESSAGES_CONFIG),
    State_1 = update_config(is_update_needed(Last_poll_datetime, Things_mtime), ?THINGS_CONFIG, State),
    State_2 = update_config(is_update_needed(Last_poll_datetime, Messages_mtime), ?MESSAGES_CONFIG, State_1),
    start_timer(),
    {noreply, State_2#state{last_poll_datetime=get_poll_time()}};


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
update_config(true, Config_file, State) ->
    lager:info("Config file : ~p must be updated!", [Config_file]),
    update_state(Config_file, config_handler:get_config(?APPLICATION, Config_file), State);
    
update_config(false, Config_file, State) ->
    State.

update_state(?MESSAGES_CONFIG, Config, State) ->
    [Pid ! {update_config, ?MESSAGES_CONFIG} ||Pid <- things_sup:get_things_pids()],
    State#state{messages=Config};
update_state(?THINGS_CONFIG, Config, State) ->
    things_sup:update_list_of_things(Config),
    State#state{things=Config}.

get_poll_time() ->
    {date(), time()}.

get_mtime(Application, Config_file) ->
    {ok, FileInfo} = file:read_file_info(filename:join([code:priv_dir(Application),"config", Config_file])),
    FileInfo#file_info.mtime.

is_update_needed(Last_poll_datetime, Fileinfo_mtime) ->
    Fileinfo_mtime >= Last_poll_datetime.

start_timer() ->
    erlang:send_after(1000, self(), update_config).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.