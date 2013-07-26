%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(dht22_sensor).

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
-export([get_type/0]).
-export([get_description/0]).
%% ====================================================================
%% External functions
%% ====================================================================
get_description() ->
    gen_server:call(?MODULE, {get_description}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {id = "0", port = 0, description = []}).
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
%% ------------------------------- -------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{id = "0", description = "Sensor, which handles the dht22"}, 0}.

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
    start_timer(),
    {noreply, State};

handle_info({call_sensor}, State=#state{id = Id}) ->
    Value = call_driver(),
    Msg = sensor:create_message(node(), ?MODULE, Id, sensor:get_seconds(), parse_message_from_dht22(Value)),
    lager:debug("got the temp and the humidity from the DHT22 ~p",[Msg]),
    sensor:send_message(nodes(),Msg),
    lager:debug("send message to the actor_group ~p",[Msg]),
    start_timer(),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("can't understand message ~p", [Info]),
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

call_driver() ->
    Driver = filename:join([code:priv_dir(horst), "driver", "dht22", "Adafruit_DHT"]),
    os:cmd(Driver ++ " 22 4").

start_timer() ->
    erlang:send_after(30000, self(), {call_sensor}). 
get_type() ->
    sensor.
parse_message_from_dht22(Msg) ->
    lager:info("Msg from DHT : ~p", [Msg]),
    Temp = case re:run(Msg ,"Temp =\s+([0-9.]+)") of 
       nomatch -> {temp, 0.0};
       {match,[{C1,C2},{C3,C4}]} -> {temp, string:substr(Msg, C3 + 1, C4)}
    end,
    Hum = case re:run(Msg ,"Hum =\s+([0-9.]+)") of 
       nomatch -> {hum, 0.0};
       {match,[{C11,C22},{C33,C44}]} -> {hum, string:substr(Msg, C33 + 1, C44)}
    end,
    [Temp, Hum].

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

parse_message_from_dht22_test() ->
    Msg = "Using pin #4\nData (40): 0x1 0xa7 0x1 0xf 0xb8\nTemp =  27.1 *C, Hum = 42.3 %",
    ?assertEqual([{temp, "27.1"},{hum, "42.3"}], parse_message_from_dht22(Msg)).

-endif.