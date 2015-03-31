-module(thing_event).

-behaviour(gen_event).

-define(NAME(), list_to_atom(atom_to_list(node()) ++ "@" ++ "thing_event_manager")).
-define(NAME(Node), list_to_atom(Node ++ "@" ++ "thing_event_manager")).
%% API
-export([start_link/1, add_handler/1, notify/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([exists/0, name/1]).

-record(state, {client :: pid()}).

%% @doc checks if the event manager exists
-spec exists() -> true | false.
exists() ->
    case global:whereis_name(?NAME()) of
        undefined -> false;
        _Pid -> true
    end.

name(_Thing) ->
    ?NAME().

%% @doc Creates an event manager
-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(Thing) ->
    gen_event:start_link({global, list_to_atom(atom_to_list(node()) ++ "@" ++ Thing)}).

%% @doc Adds an event handler. Client processes that want to receive
%% the firehose call this.
-spec add_handler(string()) -> ok | {'EXIT', term()} | term().
add_handler(Node) ->
    %% We use add_sup_handler instead of add_handler so that both the
    %% gen_event and the client process will receive notifications if
    %% either goes down.
    lager:info(".... add handler : ~p", [?NAME(Node)]),
    ok = gen_event:add_sup_handler({global, ?NAME(Node)}, ?MODULE, [self()]).

%% @doc Sends a notification to all handlers
-spec notify(term()) -> ok.
notify({Thing, Date, Value}) ->
   %% lager:info("notify : ~p ~p ~p", [Thing, Date, Value]),
    gen_event:notify({global,?NAME()}, {Thing, Date, Value}).

%% @private
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
-spec init([pid()]) -> {ok, #state{}}.
init([Client]) ->    
    {ok, #state{client = Client}}.

%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
-spec handle_event(term(), #state{}) -> {ok, #state{}}.
handle_event(Value, #state{client = Client} = State) ->
    Client ! Value,
    {ok, State};
handle_event(_Ignored, State) ->
    {ok, State}.


%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
