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
-module(thing).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([start/0]).
-export([get_type/1, get_driver/1, is_activ/1, get_timer/1, get_database/1, get_description/1]).
-export([get_state/1]).


%% ====================================================================
%% External functions
%% ====================================================================
get_type(Name) when is_list(Name)->
	get_type(list_to_atom(Name));
get_type(Name) ->
    gen_server:call(Name, {get_type}).
get_driver(Name) when is_list(Name)->
    get_driver(list_to_atom(Name));    
get_driver(Name) ->
	gen_server:call(Name, {get_driver}).
is_activ(Name) ->
	gen_server:call(list_to_atom(Name), {is_activ}).
get_timer(Name) ->
	gen_server:call(list_to_atom(Name), {get_timer}).
get_database(Name) ->
	gen_server:call(list_to_atom(Name), {get_database}).
get_description(Name) when is_list(Name) ->
    get_description(list_to_atom(Name));
get_description(Name) ->
	gen_server:call(Name, {get_description}).
get_state(Name) ->
    gen_server:call(list_to_atom(Name), {get_state}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {config, allowed_msgs}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, list_to_atom(proplists:get_value(name, Config))}, ?MODULE, Config, []).

start() ->
	start_link([]).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
    {ok, #state{config=Config, allowed_msgs = []}, 0}.

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
handle_call({get_type}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(type, Config, unknown) , State};
handle_call({get_driver}, From, State=#state{config = Config}) ->
	{driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    {reply, {Module, Module_config} , State};
handle_call({is_activ}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(activ, Config) , State};
handle_call({get_timer}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(timer, Config, 0) , State};
handle_call({get_database}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(database, Config) , State};
handle_call({get_description}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(description, Config) , State};
handle_call({get_state}, From, State) ->
    {reply, State, State};
   
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
handle_info(timeout, State=#state{config = Config}) ->
	{driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    driver_init(Module, lists:keyfind(init, 1, Module_config)),
	start_timer(proplists:get_value(timer, Config, 0)),
    {noreply, State#state{allowed_msgs = config_handler:get_messages_for_module(Module, "0")}};

handle_info([Node ,Sensor, Id, Time, Body], State=#state{allowed_msgs = Allowed_msgs, config = Config}) ->
    Config_1 = handle_msg([Node ,Sensor, Id, Time, Body], Config, sets:is_element({Node, Sensor, Id}, Allowed_msgs)),
    {noreply, State#state{config = Config_1}};

handle_info({call_sensor}, State=#state{config = Config}) ->
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func(Config),
    start_timer(proplists:get_value(timer, Config, 0)),
    {noreply, State#state{config = Config_1}};

handle_info({gpio_interrupt, 0, Pin, Status}, State=#state{config = Config}) ->   
    lager:debug("gpio_interrupt for pin : ~p with status : ~p",[Pin, Status]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func({gpio_interrupt, 0, Pin, Status}, Config, Module_config),
    {noreply, State#state{config = Config_1}};

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
handle_msg([Node ,Sensor, Id, Time, Body], Config, true) ->
    lager:info("got message : ~p : ~p", [Time, Body]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Module:Func([Node ,Sensor, Id, Time, Body], Config, Module_config);

handle_msg([Node ,Sensor, Id, Time, Body], Config, false) ->
    lager:info("got message which i don't understand : ~p", [{Node, Sensor, Id}]),
    Config.

driver_init(Module, false) ->
    lager:debug("don't init driver : ~p", [Module]);
driver_init(Module, {init, false, Config}) ->
    lager:debug("don't init driver : ~p", [Module]);
driver_init(Module, {init, true, Config}) ->
    Module:init(Config).

start_timer(0) ->
	lager:info("timer for thing  is set to 0");
start_timer(Time) ->
    erlang:send_after(Time, self(), {call_sensor}). 

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.