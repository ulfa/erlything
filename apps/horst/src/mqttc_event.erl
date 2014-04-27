%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 23.04.2014
%%%-------------------------------------------------------------------
-module(mqttc_event).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, add_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
     handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {filter}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler(Module, Arg) -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    {ok, #state{filter=Args}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({connack_accept}, State=#state{filter=Filter}) ->
lager:info("Filter : ~p", [Filter]),
    ok = emqttc:subscribe(emqttc, Filter),
    {ok, State};


handle_event({publish, Topic, Payload}, State) ->
    %%lager:info("publish: topic: ~p", [Topic]),
    %%lager:info("1a.publish: payload: ~p", [Payload]),
    Message = create_message(Topic, binary_to_term(Payload)),
    lager:info("message : ~p", [Message]),
    {ok, State};

handle_event({publish, Topic, Payload, 1, MsgId}, State) ->
    %%lager:info("publish: topic(id: ~p): ~p", [MsgId, Topic]),
    %%lager:info("publish: payload: ~p", [Payload]),
    emqttc:puback(emqttc, MsgId),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_message(Topic, {Time, Body}) ->
    [Prefix, Node, Sensor, Id] = binary:split(Topic, <<"/">>, [global]),
    [Node, Sensor, Id, list_to_binary(integer_to_list(Time)), Body].

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_message_test() ->
    T = <<"erlyThing/test@node/sensor/id">>,
    Payload = {<<1234567>>, {"sample", test}},
    ?assertEqual([<<"test@node">>, <<"sensor">>, <<"id">>, Payload], create_message(T, Payload)).
-endif.