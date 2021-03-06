%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(thing).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([get_type/1, get_driver/1, get_driver_1/1, is_activ/1, get_timer/1, get_database/1, get_description/1]).
-export([get_state/1, set_state/2, get_module_config/1, get_module_config_for_driver/1, get_start_time/1, get_name/1, get_icon/1]).
-export([save_data_to_ets/2, save_data_to_ets/3, get_table_id/1, get_model/1, set_value/2, get_value/1, get_value/2]).
-export([get_pid/1, where_is_message_from/1]).
-export([stop/1]).
-export([send_time_based/5, send_message/2]).

%% ====================================================================
%% External functions
%% ====================================================================
send_message(Name, Message) ->
    gen_server:cast(list_to_atom(Name), {send_message, Message}).

send_time_based(Time, Pid, Name, Optional, Payload) when is_pid(Pid) ->
    gen_server:cast(Pid, {send_time_based, Pid, Name, Time, Optional, Payload}).

get_value(Name) when is_list(Name) ->
    get_value(list_to_atom(Name));
get_value(Name) ->
    gen_server:call(Name, {get_value}).

get_value(Node, Name) when is_atom(Node) and is_atom(Name) ->
    rpc:call(Node,thing, get_value, [Name]).

get_pid(Name) when is_list(Name) ->
    whereis(list_to_atom(Name)); 
get_pid(Name) ->
    whereis(Name). 

set_value(Pid, Value) when is_pid(Pid) ->
    gen_server:cast(Pid, {set_value, Value}). 
get_name(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, {get_name});
get_name(undefined) ->
    undefined.
get_start_time(Name) when is_list(Name) ->
    get_start_time(list_to_atom(Name));
get_start_time(Name) ->
    gen_server:call(Name, {get_start_time}).
get_type(Name) when is_list(Name)->
	get_type(list_to_atom(Name));
get_type(Name) ->
    try 
        gen_server:call(Name, {get_type}, 100)
    catch
        _:Error -> undefined
    end.
get_icon(Name) when is_list(Name)->
    get_icon(list_to_atom(Name));
get_icon(Name) ->
    gen_server:call(Name, {get_icon}).
get_driver(Name) when is_list(Name)->
    get_driver(list_to_atom(Name));    
get_driver(Name) ->
	gen_server:call(Name, {get_driver}).
get_driver_1(Name) when is_list(Name)->
    get_driver_1(list_to_atom(Name));    
get_driver_1(Name) ->
    gen_server:call(Name, {get_driver_1}).
is_activ(Name) ->
	gen_server:call(list_to_atom(Name), {is_activ}).
get_timer(Name) ->
	gen_server:call(list_to_atom(Name), {get_timer}).
get_database(Name) ->
	gen_server:call(list_to_atom(Name), {get_database}).
get_description(Name) when is_list(Name) ->
    get_description(list_to_atom(Name));
get_description(Name) ->
	gen_server:call(Name, {get_description}).
get_state(Name) when is_list(Name) ->
    get_state(list_to_atom(Name));
get_state(Name) ->
    gen_server:call(Name, {get_state}).
get_module_config(Name) when is_list(Name) ->
    get_module_config(list_to_atom(Name));
get_module_config(Name) ->
    gen_server:call(Name, {get_module_config}).
get_model(Name) when is_list(Name) ->
    get_model(list_to_atom(Name));
get_model(Name) ->
    gen_server:call(Name, {get_model}).
set_state(Thing, State) when is_list(Thing) ->
    set_state(list_to_atom(Thing), State);
set_state(Thing, State) ->
    gen_server:call(Thing, {set_state, State}).
stop(Name) when is_list(Name) ->
    stop(list_to_atom(Name));
stop(Name) ->
    gen_server:cast(Name, {stop}).

get_module_config_for_driver(Driver) ->
    {node(),lists:flatten([thing:get_module_config(Name) || {Name, _Pid, _Type, _Modules} <- supervisor:which_children(things_sup), is_driver(Driver, thing:get_driver_1(Name))])}.

is_driver(Driver, Driver) ->
    true;
is_driver(Driver, Other) ->
    false.
%% --------------------------------------------------------------------
%% record definitions   
%% --------------------------------------------------------------------
-record(state, {config, allowed_msgs, start_time, value, timed_msgs}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, list_to_atom(config:get_value(name, Config))}, ?MODULE, Config, []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
    process_flag(trap_exit, true),
    {ok, #state{config=Config, allowed_msgs = [], start_time=0, value=undefined, timed_msgs=dict:new()}, 0}.

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
handle_call({get_value}, From, State=#state{value = Value}) ->
    {reply, Value, State};
handle_call({get_name}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(name, Config) , State};
handle_call({get_start_time}, From, State=#state{start_time = Start_time}) ->
    {reply, Start_time, State};
handle_call({get_type}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(type, Config, undefined) , State};
handle_call({get_driver}, From, State=#state{config = Config}) ->
	{driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    {reply, {Module, Module_config} , State};
handle_call({get_driver_1}, From, State=#state{config = Config}) ->
    {driver, {Driver, _Function}, Module_config} = lists:keyfind(driver, 1, Config),
    {reply, Driver , State};
handle_call({get_icon}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(icon, Config, "thing.png") , State};
handle_call({is_activ}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(activ, Config) , State};
handle_call({get_timer}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(timer, Config, 0) , State};
handle_call({get_database}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(database, Config) , State};
handle_call({get_description}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(description, Config) , State};
handle_call({get_state}, From, State) ->
    {reply, State, State};
handle_call({set_state, New_state}, From, State) ->
    {reply, State, New_state};

handle_call({get_module_config}, From, State=#state{config = Config}) ->
    case proplists:get_value(ets, Config, false) of 
        true -> Table_Id = proplists:get_value(table_id, Config),
                {reply, ets:tab2list(Table_Id) , State};
        false -> {driver, {_Module, _Func}, Module_config} = lists:keyfind(driver, 1, Config),
                 {reply, Module_config, State}
    end;
handle_call({get_model}, From, State=#state{config = Config}) ->    
    Reply = case proplists:get_value(model_fun, Config, []) of
        [] -> [];
        {Model, Function} -> F=list_to_atom(Function), 
                             Fun = fun Model:F/0,
                             Fun()
    end,
    {reply, Reply, State};   
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: save_data_to_ets/2
%% Description: Saves data to the ets of the thing
%% Returns: true
%% --------------------------------------------------------------------
save_data_to_ets(Config, Value) ->
    save_data_to_ets(Config, data, Value).

save_data_to_ets(Config, Key, Value) ->
  Table_Id = proplists:get_value(?TABLE, Config),
  ets:insert(Table_Id, [{Key, Value}]).

get_table_id(Config) ->
    proplists:get_value(?TABLE, Config).
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(die, State) ->
    exit(self(),kill),
    {noreply, State};
handle_cast({message, [Node ,Sensor, Id, Time, Optional, Payload]}, State=#state{allowed_msgs = Allowed_msgs, config = Config}) ->
    lager:debug("Message=~p ", [[Node ,Sensor, Id, Time, Optional, Payload]]),
    Config_1 = handle_msg([Node ,Sensor, Id, Time, Optional, Payload], Config, is_message_well_known({Node, Sensor, Id}, Allowed_msgs)),
    {noreply, State#state{config = Config_1}};

handle_cast({send_time_based, Pid, Name, Time, Optional, Payload}, State=#state{timed_msgs = Time_msgs}) ->
    case dict:find({Pid, Name}, Time_msgs) of 
        {ok, Timer_ref} -> erlang:cancel_timer(Timer_ref);                                                   
        error -> ok
    end,
    New_Timer_ref = erlang:send_after(Time, Pid, {send_after, Name, Optional, Payload}),
    {noreply, State#state{timed_msgs = dict:store({Pid, Name}, New_Timer_ref, Time_msgs)}};
handle_cast({set_value, Value}, State=#state{config = Config}) ->
    Name = proplists:get_value(name, Config),
    notify(thing_event:exists(), Name, now(), Value),
    {noreply, State#state{value=Value}};
handle_cast({stop}, State=#state{config = Config}) ->
    Name = proplists:get_value(name, Config), 
    lager:info("stopping thing : ~p ", [Name]),
     {stop, normal, State}; 
handle_cast({send_message, Message}, State=#state{config = Config}) ->
    {driver, {Module, Fun}, Module_config} = lists:keyfind(driver, 1, Config),
    ?SEND_1(Module, Message),
    {noreply, State};
handle_cast(Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State=#state{config = Config}) ->
	{driver, {Driver, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = ets_usage(config:get_value(ets, Config, false), Config, Module_config),
    Allowed_msgs = node_config:get_messages_for_module(Driver, config_handler:get_id(Config)),     
    Config_2 = driver_init(Driver, config:get_value(init, Module_config, false), Config_1),
	start_timer(config:get_value(timer, Config_2, 0)),
    {noreply, State#state{allowed_msgs = Allowed_msgs, start_time=now(), config = Config_2}};

%%handle_info([Node ,Sensor, Id, Time, Optional, Body], State=#state{allowed_msgs = Allowed_msgs, config = Config}) ->
%%    lager:debug("Message=~p ", [[Node ,Sensor, Id, Time, Optional, Body]]),
%%   Config_1 = handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, is_message_well_known({Node, Sensor, Id}, Allowed_msgs)),
%%    {noreply, State#state{config = Config_1}};

handle_info({call_sensor}, State=#state{config = Config}) ->
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func(Config, Module_config),
    start_timer(config:get_value(timer, Config, 0)),
    {noreply, State#state{config = Config_1}};

handle_info({gpio_interrupt, 0, Pin, Status}, State=#state{config = Config}) ->   
    lager:debug("gpio_interrupt for pin : ~p with status : ~p",[Pin, Status]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func({gpio_interrupt, 0, Pin, Status}, Config, Module_config),
    {noreply, State#state{config = Config_1}};

handle_info({external_interrupt, Application, Data_type, Body} = Msg, State=#state{config = Config}) ->   
    lager:info("external_interrupt from Application : ~p with Body : ~p",[Application, Body]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func(Msg, Config, Module_config),
    {noreply, State#state{config = Config_1}};

handle_info({update_config, ?MESSAGES_CONFIG},  State=#state{config = Config, allowed_msgs = Allowed_msgs}) ->
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config), 
    Allowed_msgs_1 = node_config:get_messages_for_module(Module, config_handler:get_id(Config)),    
    lager:info("update messages.config for thing : ~p", [Module]),
    {noreply, State#state{allowed_msgs = Allowed_msgs_1}};

handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, State=#state{config = Config}) ->
    lager:info("ETS Manager (~p) -> Thing (~p) getting TableId: ~p~n", [Pid, self(), TableId]),
    Config_1 = case lists:keysearch(table_id, 1, Config) of 
        false -> [{table_id, TableId}| Config];
        {value, Table} -> lists:keyreplace(table_id, 1, Config, {table_id, TableId})  
    end,
    {noreply, State#state{config = Config_1}};
 
handle_info({send_after, Name, Optional, Body}, State=#state{timed_msgs = Timed_msgs}) ->
    lager:info("now we send the message from : ~p with body : ~p ", [Name, Body]),
    sensor:send([], Name, Optional, Body),
    {noreply, State#state{timed_msgs = dict:erase({self(), Name}, Timed_msgs)}};
    
handle_info({Port, Payload}, State=#state{config = Config}) when is_port(Port) ->
    lager:info("got a message from a port with payload: ~p ", [Payload]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func(Payload, Config, Module_config),        
    {noreply, State#state{config = Config_1}};

handle_info({'EXIT', Port, normal}, State) ->
    {noreply, State};  

handle_info(Info, State) ->
    lager:error("~p got message : ~p that i don't understand. ~p", [?MODULE, Info, State]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
   terminate(Reason, State=#state{config = Config}) ->
    lager:info("Reason for termination : ~p",[Reason]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config), 
    lager:info("stopping thing of type : ~p", [Module]),
    Exports = config:get_value(exports, Module:module_info(), []),
    case config:get_value(stop, Exports) of 
        undefined -> lager:warning("there is no stop function in module : ~p", [Module]);
        1 -> driver_stop(Module, Config);
        Any -> lager:warning("the stop function has too many arguments")
    end,
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
notify(true, Name, Date, []) ->
    %%lager:info(".... ~p", [Name]),
    false;
notify(true, Name, Date, Value) ->
    thing_event:notify({Name, Date, Value});
notify(false, Name, Date, Value) ->
    false.

is_message_well_known({Node, Sensor, Id}, Allowed_msgs) ->
    case  sets:is_element({Node, Sensor, Id}, Allowed_msgs) of 
        true -> true;
        false -> is_msgs_allowed({Node, Sensor, Id}, sets:to_list(Allowed_msgs))
    end.

is_msgs_allowed({Node, Sensor, Id}, [Check|Checks]) ->
    case is_msgs_allowed({Node, Sensor, Id}, Check) of 
        true -> true;
        false -> is_msgs_allowed({Node, Sensor, Id}, Checks)
    end;
is_msgs_allowed({Node, Sensor, Id}, {all, all, all}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {all, Sensor, all}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {all, Sensor1, all}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, {all, all, Id}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {all, all, Id1}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, {Node, all, all}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {Node1, all, all}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, {Node, Sensor, all}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {Node1, Sensor1, all}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, {Node, all, Id}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {Node1, all, Id1}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, {all, Sensor, Id}) ->
    true;
is_msgs_allowed({Node, Sensor, Id}, {all, Sensor1, Id1}) ->
    false;

is_msgs_allowed({Node, Sensor, Id}, Allowed_msgs) ->
    false.

handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, true) ->
    lager:debug("got message : ~p : ~p", [Time, Body]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Module:Func([Node ,Sensor, Id, Time, Optional, Body], Config, Module_config);

handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, false) ->
    lager:debug("got message which i don't understand : ~p", [{Node, Sensor, Id}]),
    Config.

driver_init(Module, false, Config) ->
    lager:debug("don't init driver : ~p", [Module]),
    check_init(Module),
    Config;
driver_init(Module, true, Config) ->
    lager:debug("call init for driver : ~p", [Module]),    
    {ok, Config_1} = Module:init(Config),
    Config_1.

driver_stop(Module, Config) ->
    lager:debug("call stop for driver : ~p", [Module]),    
    {ok, Config_1} = Module:stop(Config),
    Config_1.

start_timer(0) ->
	lager:info("timer for thing  is set to 0");
start_timer(Time) ->
    erlang:send_after(Time, self(), {call_sensor}). 
    
ets_usage(true, Config, Module_config) ->
    Table_Id = create_ets(Config, Module_config),
    lager:info("owned from ets manager the table : ~p", [Table_Id]),
    [{table_id, Table_Id}|Config];
ets_usage(false, Config, _Module_config) ->
    Config.
    
create_ets(Config, Module_config) ->
    Name = config:get_value(name, Config),
    Id = config_handler:get_id(Config),
    ets_mgr:init_table(self(), list_to_atom(Name ++ "_" ++ Id), Module_config).  

check_init(Module) ->
    Exports = config:get_value(exports, Module:module_info(), []),
    case config:get_value(init, Exports) of 
        undefined -> true;
        1 -> lager:warning("there is a init function in the module '~p', but it is false or not available in the config", [Module]),
             false;
        Any -> lager:warning("the init function has too many arguments"),
                false
    end.
where_is_message_from([Node, Driver, Id, Optional, Payload]) ->    
    case config_handler:get_thing_name(Optional) of 
        [] -> [];
        Name -> rpc:call(binary_to_existing_atom(Node, utf8), erlang, whereis, [Name])
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
is_message_well_known_test() ->
    Allowed_msgs_1 = sets:from_list([{<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, {all, all, all}]),
    Allowed_msgs_2 = sets:from_list([{all, all, all}]),
    Allowed_msgs_3 = sets:from_list([{all, all, <<"0">>}]),
    ?assertEqual(true, is_message_well_known({<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_1)),
    ?assertEqual(true, is_message_well_known({<<"test@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_2)),
    ?assertEqual(true, is_message_well_known({<<"test@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_3)).

is_msgs_allowed_test() ->
    Allowed_msgs_1 = [{all, <<"hc_sr501_driver">>, all}],
    ?assertEqual(true, is_msgs_allowed({<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_1)),
    ?assertEqual(false, is_msgs_allowed({<<"horst@raspberrypi">>,<<"cube_driver">>,<<"0">>}, Allowed_msgs_1)).
-endif.