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
-module(hc_sr501_sensor).

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
%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {activ=false, last_changed}).
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
    {ok, #state{activ=false, last_changed=[]}, 0}.

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
handle_info(timeout, State) ->
    gpio:set_interrupt(7, both),
    {noreply, State};
handle_info({gpio_interrupt, 0, 7, 0}, State) ->
    Msg = create_msg(node(), ?MODULE, get_seconds(), "FALLING"),
    %%send_message(Msg),
    lager:debug("gpio_interrupt FALLING ~p",[Msg]),
    {noreply, State#state{activ=true}};
handle_info({gpio_interrupt, 0, 7, 1}, State) ->
    Msg = create_msg(node(), ?MODULE, get_seconds(), "RISING"),
    send_message(Msg),
    lager:debug("gpio_interrupt RISING ~p",[Msg]),
    {noreply, State#state{activ=false}};

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
send_message(Message) ->
    lager:debug("sending message to these nodes : ", [nodes()]),
    rpc:abcast(nodes(), 'actor_group', Message).

create_msg(Node, Sensor, Time, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Sensor, utf8), list_to_binary(integer_to_list(Time)), erlang:list_to_binary(Body)].

get_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

handle_info_test() ->
    ?assertEqual({noreply, #state{activ=true}}, ?MODULE:handle_info({gpio_interrupt, 0, 7, 0}, #state{activ=false})).


create_msg_test() ->
    ?assertEqual([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"63540684780">>,<<"FALLING">>], create_msg('horst@notebook', 'hc_sr501_sensor', 63540684780, "FALLING")).
-endif.