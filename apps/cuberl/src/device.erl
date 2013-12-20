%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(device).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([set_l_data/2, get_l_data/1]).
-export([set_c_data/2, get_c_data/1]).

%% ====================================================================
%% External functions
%% ====================================================================
set_l_data(RF_address, L_data) ->
	lager:info("set_l_data : ~p", [L_data]),
	gen_server:cast(int_to_atom(RF_address), {set_l_data, L_data}).
get_l_data(RF_address) ->
	gen_server:call(int_to_atom(RF_address), {get_l_data}).

set_c_data(RF_address, C_data) ->
	gen_server:cast(int_to_atom(RF_address), {set_c_data, C_data}).
get_c_data(RF_address) ->
	gen_server:call(int_to_atom(RF_address), {get_c_data}).


%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {config, l_data, c_data}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, int_to_atom(proplists:get_value(rf_address, Config))}, ?MODULE, Config, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
    {ok, #state{config = Config}}.

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
handle_call({get_l_data}, From, State=#state{l_data = L_data}) ->
    {reply, L_data, State};

handle_call({get_c_data}, From, State=#state{c_data = C_data}) ->
    {reply, C_data, State};

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
handle_cast({set_l_data, L_data}, State) ->
    lager:info("adasdsadsd"),
    set_live_data(L_data),
    {noreply, State#state{l_data = L_data}};

handle_cast({set_c_data, C_data}, State) ->
	lager:info("set c_data : ~p", [C_data]),
    set_config_data(proplists:get_value(device_type, C_data), C_data),
    {noreply, State#state{c_data = C_data}};

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
%% Thermostat
set_live_data(Data) ->
    cuberl_sender:send_message({external_event, cuberl,  [{type, live_data}, {data, Data}]}).

set_config_data(1, Data) ->
    cuberl_sender:send_message({external_event, cuberl,  [{type, config_data}, {device_type, 1}, {data, Data}]});
set_config_data(3, Data) ->
    cuberl_sender:send_message({external_event, cuberl,  [{type, config_data}, {device_type, 3}, {data, Data}]});
set_config_data(4, Data) ->
    cuberl_sender:send_message({external_event, cuberl,  [{type, config_data}, {device_type, 4}, {data, Data}]});
set_config_data(5, Data) ->
    cuberl_sender:send_message({external_event, cuberl,  [{type, config_data}, {device_type, 5}, {data, Data}]});
set_config_data(Device_type, Data) ->
    lager:warning("don't understand config data : ~p", [Data]).

int_to_atom(Int) ->
	list_to_atom(integer_to_list(Int)).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).    
-endif.