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
-include("../include/cuberl.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([set_l_data/2, get_l_data/1]).
-export([set_c_data/2, get_c_data/1]).
-export([get_model/1]).

%% ====================================================================
%% External functions
%% ====================================================================
set_l_data(RF_address, L_data) ->
	lager:debug("set_l_data : ~p", [L_data]),
	gen_server:cast(int_to_atom(RF_address), {set_l_data, L_data}).
get_l_data(RF_address) ->
	gen_server:call(int_to_atom(RF_address), {get_l_data}).

set_c_data(RF_address, C_data) ->
	gen_server:cast(int_to_atom(RF_address), {set_c_data, C_data}).
get_c_data(RF_address) ->
	gen_server:call(int_to_atom(RF_address), {get_c_data}).
get_model(RF_address) when is_atom(RF_address) ->
    gen_server:call(RF_address, {get_model, RF_address}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {config=[], l_data=[], c_data=[]}).
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

handle_call({get_model, RF_address}, From, State=#state{config = Config, l_data = L_data, c_data = C_data}) ->
    Model = {RF_address, [{config, Config}, {l_data, L_data}, {c_data, C_data}]},
    {reply, Model, State};
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
handle_cast({set_l_data, L_data}, State=#state{c_data = C_data, l_data = Old_l_data}) ->
    set_live_data(get(device_type, C_data), C_data, Old_l_data, L_data),
    {noreply, State#state{l_data = L_data}};

handle_cast({set_c_data, C_data_new}, State=#state{c_data = C_data}) ->
    set_config_data(get(device_type, C_data_new), C_data_new, C_data),
    {noreply, State#state{c_data = C_data_new}};

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

set_live_data(Device_type, C_data, Data, Data) ->
    lager:debug("nothing changed for device_type : ~p data: ~p", [proplists:get_value(device_type, C_data), Data]);
set_live_data(1, C_data, Old_l_data, Data) ->
    data_changed(act_temp, C_data, Data, Old_l_data);
set_live_data(3, C_data, Old_l_data, Data) ->
    data_changed(act_temp, C_data, Data, Old_l_data);
set_live_data(4, C_data, Old_l_data, Data) ->
    data_changed(window, C_data, Data, Old_l_data);
set_live_data(5, C_data, Old_l_data, Data) ->
    data_changed(act_temp, C_data, Data, Old_l_data).

set_config_data(1, New_data, C_data) ->
    cuberl_sender:send_message(?MESSAGE_CONFIG([{type, config_data}, {device_type, values:value(device_type, 1)}, {data, New_data}]));
set_config_data(3, New_data, C_data) ->
    cuberl_sender:send_message(?MESSAGE_CONFIG([{type, config_data}, {device_type, values:value(device_type, 3)}, {data, New_data}]));
set_config_data(4, New_data, C_data) ->
    cuberl_sender:send_message(?MESSAGE_CONFIG([{type, config_data}, {device_type, values:value(device_type, 4)}, {data, New_data}]));
set_config_data(5, New_data, C_data) ->
    cuberl_sender:send_message(?MESSAGE_CONFIG([{type, config_data}, {device_type, values:value(device_type, 5)}, {data, New_data}]));
set_config_data(Device_type, New_data, C_data) ->
    lager:warning("don't understand config data : ~p", [New_data]).

int_to_atom(Int) ->
	list_to_atom(integer_to_list(Int)).

data_changed(Something, C_data, New_data, New_data) ->
    lager:warning("No changes on live data for unknown: ~p with data : ~p", [Something, New_data]);
data_changed(act_temp, C_data, New_data, Data) ->
    Meta = get_meta(C_data),
    Temp = get(act_temp, New_data),
    cuberl_sender:send_message(?MESSAGE_LIVE([{act_temp_state, Temp} | Meta]));
data_changed(window, C_data, New_data, Data) ->
    Meta = get_meta(C_data),
    Window_state = get(window, New_data),
    cuberl_sender:send_message(?MESSAGE_LIVE([{window_state, Window_state}| Meta]));
data_changed(battery, C_data, New_data, Data) ->
    Meta = get_meta(C_data),
    Battery_state = proplists:get_value(battery, New_data),
    cuberl_sender:send_message(?MESSAGE_LIVE([{battery_state, unknown_yet} | Meta])).

get_meta(C_data) ->
    Device_type = get(device_type, C_data), 
    Device_name = values:value(device_type, Device_type),
    Room_id = get(room_id, C_data), 
    Room_name = get(room_name, Room_id),
    [{room_id, Room_id}, {room_name, Room_name}, {device_type, Device_type}, {device_name, Device_name}].

get(room_name, Room_id) ->
    room:get_name(Room_id);

get(Key, List) ->
    proplists:get_value(Key, List).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).    
list_equal_test() ->
    A = lists:keysort(1,[{a, "1"}, {b, "2"}, {c, "3"}]),
    B = lists:keysort(1,[{b, "2"}, {a, "1"}, {c, "3"}]),
    C = [{a, "1"}, {b, "2"}, {c, "3"}],
    D = [{b, "2"}, {a, "1"}, {c, "3"}],
    ?assertEqual(true, A == B),
    ?assertEqual(true, C /= D).

send_config_data_test() ->
    A = lists:keysort(1,[{a, "1"}, {b, "2"}, {c, "3"}]),
    B = lists:keysort(1,[{b, "2"}, {a, "1"}, {c, "3"}]),
    set_config_data(3, A, B).

-endif.