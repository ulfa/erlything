%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(transmitter_433_actor).

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
-export([get_description/0]).
-export([switch/2, get_switched/1, get_list_of_switches/0]).

-define(ON, "1").
-define(OFF, "0").

%% ====================================================================
%% External functions
%% ====================================================================
get_description() ->
    gen_server:call(?MODULE, {get_description}).

switch(Switch, Status) ->
    gen_server:cast(?MODULE, {switch, Switch, Status}).

get_switched(Switch) ->
    gen_server:call(?MODULE, {get_switched, Switch}).

get_list_of_switches() ->
    gen_server:call(?MODULE, {get_list_of_switches}).


%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {id, switched, description}).
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
    {ok, #state{id="0", switched=[{"1", ?OFF},{"2", ?OFF},{"3", ?OFF},{"4", ?OFF}], description="Actor, which can switch on/off plug sockets"}}.

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
handle_call({get_description}, From, State=#state{description = Description}) ->
    {reply, Description, State};

handle_call({get_switched, Switch}, From, State=#state{switched = List}) ->
    R = case lists:keyfind(Switch,1, List) of 
        false -> {error, "not a valid switch number"};
        {S, V} -> V 
    end,
    {reply, R, State};

handle_call(Request, From, State=#state{switched = List}) ->
    {reply, {node(),List}, State};

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
handle_cast({switch, Switch, Status}, State) ->
    lager:debug("switching plug number : ~p to State ~p" , [Switch, Status]),
    State_1 = switch(Switch, Status, State),
    {noreply, State_1};

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
switch(Switch, ?ON, State=#state{switched = List}) ->
    lager:debug("switch number ~p on", [Switch]),
    switch_1(Switch, ?ON),
    State#state{switched=save_state(Switch, ?ON, List)};
switch(Switch, ?OFF, State=#state{switched = List}) ->
    lager:debug("switch number ~p off", [Switch]),
    switch_1(Switch, ?OFF),
    State#state{switched=save_state(Switch, ?OFF, List)}.

switch_1(Switch, Status) ->
    Driver = filename:join([code:priv_dir(horst), "driver", "remote", "send"]),
    os:cmd(Driver ++ " 11111 " ++ Switch ++ " " ++ Status).

save_state(Switch, Status, List) ->
    lists:keyreplace(Switch,1, List, {Switch, Status}).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).


-endif.