%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(seven_eleven_actor).

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
-export([get_description/0, get_id/0]).

%% ====================================================================
%% External functions
%% ====================================================================
get_description() ->
    gen_server:call(?MODULE, {get_description}).

get_id() ->
    gen_server:call(?MODULE, {get_id}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {id, allowed_msgs=[], description=[]}).
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
    {ok, #state{id="0", allowed_msgs=[], description="Actor, which rings the 7-11 bell"}, 0}.

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

handle_call({get_id}, From, State=#state{id = Id}) ->
    {reply, Id, State};

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
    {noreply, State#state{allowed_msgs = config_handler:get_messages_for_module(?MODULE, "0")}};

handle_info([Node ,Sensor, Id, Time, Body], State=#state{allowed_msgs = Allowed_msgs}) ->
    case sets:is_element({Node, Sensor, Id}, Allowed_msgs) of 
        false ->  lager:debug("got message which i don't understand : ~p", [{Node, Sensor, Id}]);
        true -> play_sound()
    end,
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
play_sound() ->
    lager:info("let's ring the bell!"),
    play_sound(os:type()).

play_sound({unix,linux}) ->
	os:cmd("mpg123 " ++ filename:append(code:priv_dir(horst), "sounds/7-11.mp3"));
play_sound({unix, darwin}) ->
    os:cmd("afplay " ++ filename:append(code:priv_dir(horst), "sounds/7-11.mp3"));
play_sound({A, B}) ->
    lager:error("I don't know this system. Please, inform the author ~p", [{A,B}]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

handle_info_test() ->
	?assertEqual({noreply, #state{}}, handle_info([<<"horst@ronja">>,<<"hc_sr501_sensor">>,<<"0">>, <<"63540749293">>,"RISING"],#state{})).
-endif.