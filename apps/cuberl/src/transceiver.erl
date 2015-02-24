%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(transceiver).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/cuberl.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([get_live_data/0]).
-export([connect/2, disconnect/0]).
-export([send_command/1]).
%% ====================================================================
%% External functions
%% ====================================================================
connect(Ip, Port) ->
    gen_server:call(?MODULE, {connect, Ip, Port}).

disconnect() ->
    gen_server:call(?MODULE, {disconnect}).

get_live_data() ->
	gen_server:cast(?MODULE, {get_live_data}).

send_command(Command) ->
    gen_server:cast(?MODULE, {send_command, Command}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {socket, listenSocket}).
-record(room, {}).

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
    {ok, #state{}, 0}.

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
handle_call({connect, Ip, Port}, _From, State) ->
    lager:info("connecting to the cube with ip : ~p and on port : ~p", [Ip, Port]),
    cuberl_sender:send_message(?MESSAGE_INFO("connecting to the cube")),
    {ok, Socket} = gen_tcp:connect(Ip, Port, []),
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    {reply, ok, #state{socket = Socket, listenSocket = ListenSocket}};

handle_call({disconnect}, _From, State=#state{socket = Socket}) ->
    lager:info("disconnecting from the cube"),
    cuberl_sender:send_message(?MESSAGE_INFO("disconnecting from the cube")),
    close(Socket), 
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------	
handle_cast({get_live_data}, State=#state{socket = Socket}) ->
	lager:debug("sending info message to the cube"),	
	send_command(Socket, "l:"), 
    {noreply, State};

handle_cast({send_command, Command}, State=#state{socket = Socket}) ->
    lager:debug("sending command : ~p to the cube.", [Command]), 
    send_command(Socket, Command), 
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) ->
    lager:debug("Data :~p", [Data]),
	message_handler:handle_message(list_to_binary(Data)),
	{noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    lager:info("Socket closed by peer"),
    cuberl_sender:send_message(?MESSAGE_ERROR("Socket closed by peer")),
    close(Socket),
    {noreply, State};

handle_info({tcp_closed, undefined}, State) ->
    lager:info("Socket is undefined, please check your cube!"),
    cuberl_sender:send_message(?MESSAGE_ERROR("Socket is undefined, please check your cube!")),
    {noreply, State};

handle_info(timeout, State) ->	
    Port = get_env(port),
    Ip = get_env(ip),
    case gen_tcp:connect(Ip, Port, []) of 
        {ok, Socket} -> {ok, ListenSocket} = gen_tcp:listen(Port, []),
                        start_timer(get_env(timer)),
	                    {noreply, #state{socket = Socket, listenSocket = ListenSocket}};
        {error, Reason} -> cuberl_sender:send_message({external_interrupt, cuberl, error, {fatal, Reason}}),
                           lager:error("can't connect to the cube, because of : ~p", [Reason]),
                           cuberl_sender:send_message(?MESSAGE_ERROR("can't connect to the cube, because of", Reason)),
                           {noreply, State}
    end; 

handle_info({get_live_data}, State=#state{socket = Socket}) ->
    send_command(Socket, "l:"),
    start_timer(get_env(timer)),    
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State=#state{socket = Socket}) ->
	close(Socket), 
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
close(undefined) ->
    lager:error("Socket is undefined, please check your cube!");
close(Socket) ->
    gen_tcp:close(Socket).

send_command(Socket, Command) ->
    lager:debug("Command : ~p", [Command]),
    gen_tcp:send(Socket, Command ++ "\r\n").

get_env(Key) ->
	{ok, Value} = application:get_env(cuberl, Key),
    lager:info("Key : ~p Value: ~p", [Key, Value]),
	Value.

start_timer(Time) ->
    erlang:send_after(Time, self(), {get_live_data}).     
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).		
-endif.
