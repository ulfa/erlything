%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(room).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([get_name/1]).
-export([add_device/2, get_devices/1]).
-export([get_model/1]).
%% ====================================================================
%% External functions
%% ====================================================================
add_device(Room_id, Device_config) ->
    gen_server:cast(list_to_atom(integer_to_list(Room_id)), {add_device, Device_config}).

get_devices(Room_id) ->
    gen_server:call(list_to_atom(integer_to_list(Room_id)), {get_devices}).

get_model(Room_id) when is_atom(Room_id)->
    gen_server:call(Room_id, {get_model, Room_id}).   
get_name(Room_id) ->
    gen_server:call(list_to_atom(integer_to_list(Room_id)), {get_name}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {config, devices=[]}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, list_to_atom(integer_to_list(proplists:get_value(room_id, Config)))}, ?MODULE, Config, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
    {ok, #state{config=Config, devices=[]}}.

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
handle_call({get_name}, From,  State=#state{config = Config}) ->
    {reply, proplists:get_value(room_name, Config) , State};

handle_call({get_devices}, From,  State=#state{devices = Devices}) ->
    {reply, Devices, State};

handle_call({get_model, Room_id}, From, State=#state{config = Config, devices = Devices}) ->
    Devices_model = [device:get_model(int_to_atom(Device))|| Device <- Devices],
    Model = {Room_id, [{config, Config}, {devices, Devices_model}]},
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
handle_cast({add_device, Config}, State=#state{devices = Devices}) ->
    house_sup:start_device([{room_id, proplists:get_value(room_id, Config)}|Config]), 
    {noreply, State#state{devices = [proplists:get_value(rf_address, Config) |Devices]}};

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
int_to_atom(Int) ->
    list_to_atom(integer_to_list(Int)).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.