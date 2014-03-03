%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(node).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sue.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").
%% --------------------------------------------------------------------
%% External exports
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3, start/1]).

-export([get_status/1, sys_info/1, etop/1, memory/1, set_alive/1, pid_info/2]).
-export([get_applications/1, get_app_info/2]). 

%% ====================================================================
%% External functions
%% ====================================================================
get_app_info(Node, App) ->
	gen_server:call(Node, {app_info, Node, App}).

get_applications(Node) ->
	gen_server:call(Node, {applications, Node}).

set_alive(Node) when is_binary(Node)->
	set_alive(binary_to_atom(Node, utf8));
set_alive(Node) when is_atom(Node)->
	gen_server:cast(Node, set_alive).
	
memory(Node) ->
	memory1(Node).

sys_info(Node) ->
	sys_info1(Node).	
	
etop(Node) when is_atom(Node)->
	gen_server:call(Node, {etop, Node}).

pid_info(Node, Pid) when is_atom(Node)->
	gen_server:call(Node, {pid_info, Node, Pid}).
	
get_status(Node) when is_pid(Node)->
	gen_server:call(Node, get_state);
	
get_status(Node) when is_atom(Node)->
	gen_server:call(Node, get_state).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {status = ?UNKNOWN, node, time, ip, reason=[], uptime}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Node, Ip, Uptime) ->
    gen_server:start_link({local, Node}, ?MODULE, [Node, Ip, Uptime], []).
	
start([Node, Ip, Uptime]) ->
	start_link(Node, Ip, Uptime).	
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Node, Ip, Uptime]) ->
	net_kernel:monitor_nodes(true, [nodedown_reason]),		
	start_timer(Node),
    {ok, #state{node = erlang:atom_to_binary(Node, utf8), ip = Ip, time = get_timestamp(), uptime=Uptime}}.
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
handle_call(get_state, From, #state{status = Status, ip = Ip, time = Time, node = Node, reason = Reason, uptime = Uptime} = State) -> 
    {reply, {Node, [{ip, ip_device:ip_as_string(Ip)}, {state, Status}, {time, date:timestamp_to_date(Time)}, 
    {reason, Reason}]}, State};

handle_call({app_info, Node, App}, From, State) ->
	Reply = get_app_info1(Node, App),
	{reply, Reply, State};

handle_call({applications, Node}, From, State) ->
	Reply = get_applications2(Node, application:loaded_applications()),
	{reply, Reply, State};

handle_call({etop, Node}, From, State) ->
	Reply = etop1(Node),
	{reply, Reply, State};

handle_call({pid_info, Node, Pid}, From, State) ->
	Reply = pid_info1(Node, Pid),
	{reply, Reply, State};	
handle_call(get_name, From, #state{node = Node} = State) ->
    {reply, Node, State};
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
handle_cast(set_alive, State) ->
	{noreply, State#state{status=?ALIVE, time=get_timestamp(), reason=[]}};
	
handle_cast(Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({update, Node}, #state{status=Old_s}=State) ->
	lager:debug("update node : ~p from state : ~p ", [Node, Old_s]),
	New_s = ping_node(Node),
	start_timer(Node),
	case New_s =:= Old_s of 
		true -> {noreply, State};
		false -> {noreply, State#state{status=New_s, time=get_timestamp(), reason=[]}}
	end;
	
handle_info({nodeup, Node, InfoList}, #state{node = Node1} = State) ->
	lager:debug("nodeup : ~p ~p", [Node, InfoList]),
	case erlang:atom_to_binary(Node, utf8) =:= Node1 of
		true -> {noreply, State#state{status=?ALIVE, reason=InfoList, time=get_timestamp()}};
		false -> {noreply, State}
	end;
handle_info({nodedown, Node, InfoList}, #state{node = Node1} = State) ->
	lager:debug("nodedown : ~p, ~p", [Node, InfoList]),
	case erlang:atom_to_binary(Node, utf8) =:= Node1 of
		true -> {noreply, State#state{status=?DEAD, reason=InfoList, time=get_timestamp()}};
		false -> {noreply, State}
	end;

handle_info(Info, State) ->
	lager:warning("got a message i can't handle info: ~p in state : ~p", [Info, State]),
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
get_app_info1(Node, App) ->
	Processes = process_info:get_processes(App, all, Node),
	convert_children(Processes).

get_applications2(Node, Apps) ->
	[{Node,[], [], ''}|[{App, Version, Tool, Node} || {App, Tool, Version} <- Apps]].	

etop1(Node) ->
	case rpc:call(Node, sue_etop, collect, []) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 

pid_info1(Node, Pid) ->
	case rpc:call(Node, sue_etop, pid_info, [Pid]) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 

sys_info1(Node) ->
	case rpc:call(Node, observer_backend, sys_info, []) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 
	
memory1(Node) ->
	rpc:call(Node, erlang, memory, []).
	
start_timer(Node) ->
	erlang:send_after(10000, whereis(Node), {update, Node}).		
	
ping_node(Node) when is_binary(Node) ->
	ping_node(erlang:binary_to_atom(Node, utf8));
ping_node(Node)  ->
	case net_adm:ping(Node) of
		pang -> get_state(pang); 
		pong -> get_state(pong)
	end.
	
get_state(pang) ->
	?DEAD;
get_state(pong) ->
	?ALIVE.

get_timestamp() ->
    date:get_timestamp().


convert_children(unknown) ->
	[];

convert_children([{Parent, Children, []}, Num]) ->
	[[get_name(Parent), '']|convert_children({Parent, Children, []}, [])];

convert_children({{Parent, Children, []}, Num}) ->	
	[[get_name(Parent), '']|convert_children({Parent, Children, []}, [])];
convert_children({{Parent, Children, []}, Num}) ->
	[[get_name(Parent), '']|convert_children({Parent, Children, []}, [])].

convert_children({Parent, [], []}, Acc) ->	
	Acc;

convert_children({Parent, [], []}, Acc) ->
	Acc;

convert_children({Parent, [{Name, Children, Ignore}|T], []}, Acc) ->		
	List = convert_children({Name, Children, []}, []),	
	Acc1 = lists:append(List, Acc), 
	convert_children({Parent, T, []}, [[get_name(Name),get_name(Parent)]|Acc1]).

get_name(Name_pid) ->
	Name = lists:nth(1,string:tokens(Name_pid, ":")),
	string:strip(Name, both).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_applications1_test() ->
A =[{mnesia,"MNESIA  CXC 138 12","4.7.1"},
	{mimetypes,"mimetypes","0.9-5-geaf7c84"},
	{sue,[],"0.1.0"},
	{kernel,"ERTS  CXC 138 10","2.15.3"},
	{crypto,"CRYPTO version 2","2.2"},
	{erlcron,"Erlang Implementation of cron","0.3.0"},
	{sasl,"SASL  CXC 138 11","2.2.1"},
	{boss,"Chicago Boss web framework, now serving three flavors of Comet", "0.8.0"},
 	{stdlib,"ERTS  CXC 138 10","1.18.3"},
 	{tinymq,"TinyMQ: a diminutive message queue","0.1.0"}],

?assertEqual([], get_applications2('sue', A)).
	
convert_app_info_test() ->
A={{"<0.77.0>",
  [{"tranceiver_sup : <0.80.0>",
    [{"sue_sup : <0.79.0>", [{"node_sup : <0.82.0>",[{"sue@kiezkantine : <0.129.0>",[],[]},{"moni@ua-TA880GB : <0.142.0>",[],[]}],[]}, {"<0.78.0>",[],[]}],[]},
     {"tranceiver : <0.81.0>", [{"Port :#Port<0.2339>",[],[]},{"Port :#Port<0.2325>",[],[]}],[]}],[]}
  ],[]},
 53277}.


 convert_children_test() ->
 	A= {{"<0.77.0>", [{"tranceiver_sup : <0.80.0>",  [{"sue_sup : <0.79.0>",  [{"node_sup : <0.82.0>",[{"sue@kiezkantine : <0.129.0>",[],[]},
 																{"moni@ua-TA880GB : <0.142.0>",[],[]}],[]}, {"<0.78.0>",[],[]}],[]}], []}],[]},53277},
 	?assertEqual([["<0.77.0>",''],["tranceiver_sup","<0.77.0>"],["sue_sup", "tranceiver_sup"], ["<0.78.0>","sue_sup"], ["node_sup", "sue_sup"], ["moni@ua-TA880GB", "node_sup"], ["sue@kiezkantine", "node_sup"]], convert_children(A)).

convert_lager_test() ->
	A=[{"<0.47.0>",
          [{"lager_event : <0.52.0>",
                      [{"lager_sup : <0.49.0>",
                        [{"lager_handler_watcher_sup : <0.53.0>",
                          [{"<0.55.0>",[],[]},
                           {"<0.56.0>",[],[]}],
                          []},
                         {"lager_crash_log : <0.54.0>",
                          [{"Port :#Port<0.1538>",[],[]}],
                          []},
                         {"<0.48.0>",[],[]}],
                        []},
                       {"Port :#Port<0.1575>",[],[]},
                       {"Port :#Port<0.1574>",[],[]}],
                      ["<0.55.0>","<0.56.0>","<0.57.0>"]}],
                    []},
                   []],
     ?assertEqual([], convert_children(A)).
-endif.


