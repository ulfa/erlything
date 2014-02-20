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
-export([start/0]).
-export([get_type/1, get_driver/1, is_activ/1, get_timer/1, get_database/1, get_description/1]).
-export([get_state/1, get_module_config/1, get_start_time/1, get_name/1, get_icon/1]).
-export([save_data_to_ets/2, save_data_to_ets/3, get_table_id/1, get_model/1]).
-export([stop/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% only to demonstration for the ets stuff
get_name(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, {get_name}).
get_start_time(Name) when is_list(Name) ->
    get_start_time(list_to_atom(Name));
get_start_time(Name) ->
    gen_server:call(Name, {get_start_time}).
get_type(Name) when is_list(Name)->
	get_type(list_to_atom(Name));
get_type(Name) ->
    gen_server:call(Name, {get_type}).
get_icon(Name) when is_list(Name)->
    get_icon(list_to_atom(Name));
get_icon(Name) ->
    gen_server:call(Name, {get_icon}).
get_driver(Name) when is_list(Name)->
    get_driver(list_to_atom(Name));    
get_driver(Name) ->
	gen_server:call(Name, {get_driver}).
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
get_state(Name) ->
    gen_server:call(list_to_atom(Name), {get_state}).
get_module_config(Name) when is_list(Name) ->
    get_module_config(list_to_atom(Name));
get_module_config(Name) ->
    gen_server:call(Name, {get_module_config}).
get_model(Name) when is_list(Name) ->
    get_model(list_to_atom(Name));
get_model(Name) ->
    gen_server:call(Name, {get_model}).

stop(Name) when is_list(Name) ->
    stop(list_to_atom(Name));
stop(Name) ->
    gen_server:cast(Name, {stop}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {config, allowed_msgs, start_time}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, list_to_atom(proplists:get_value(name, Config))}, ?MODULE, Config, []).

start() ->
	start_link([]).
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
    {ok, #state{config=Config, allowed_msgs = [], start_time=0}, 0}.

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
handle_call({get_name}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(name, Config) , State};
handle_call({get_start_time}, From, State=#state{start_time = Start_time}) ->
    {reply, Start_time, State};
handle_call({get_type}, From, State=#state{config = Config}) ->
    {reply, proplists:get_value(type, Config, unknown) , State};
handle_call({get_driver}, From, State=#state{config = Config}) ->
	{driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    {reply, {Module, Module_config} , State};
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
    exit(self(),killed),
    {noreply, State};

handle_cast({stop}, State=#state{config = Config}) ->
    Name = proplists:get_value(name, Config), 
    lager:info("stopping thing : ~p ", [Name]),
     {stop, normal, State}; 
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
	{driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = ets_usage(proplists:get_value(ets, Config, false), Config, Module_config),
    Allowed_msgs = node_config:get_messages_for_module(Module, config_handler:get_id(Config)),     
    driver_init(Module, proplists:get_value(init, Module_config, false), Config_1),
	start_timer(proplists:get_value(timer, Config_1, 0)),
    {noreply, State#state{allowed_msgs = Allowed_msgs, start_time=now(), config = Config_1}};

handle_info([Node ,Sensor, Id, Time, Body], State=#state{allowed_msgs = Allowed_msgs, config = Config}) ->
    lager:debug("Message=~p ", [[Node ,Sensor, Id, Time, Body]]),
    Config_1 = handle_msg([Node ,Sensor, Id, Time, Body], Config, is_message_well_known({Node, Sensor, Id}, Allowed_msgs)),
    {noreply, State#state{config = Config_1}};

handle_info({call_sensor}, State=#state{config = Config}) ->
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:Func(Config, Module_config),
    start_timer(proplists:get_value(timer, Config, 0)),
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

handle_info(Info, State) ->
    lager:info("got message : ~p that i don't understand.", [Info]),
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
    Exports = proplists:get_value(exports, Module:module_info(), []),
    case proplists:get_value(stop, Exports) of 
        undefined -> lager:warning("there is no stop function in module : ~p", [Module]);
        1 -> Module:stop(Config);
        Any -> lager:waring("the stop function has too many arguments")
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
is_message_well_known({Node, Sensor, Id}, Allowed_msgs) ->
    case  sets:is_element({Node, Sensor, Id}, Allowed_msgs) of 
        true -> true;
        false -> are_all_msgs_allowed(sets:to_list(Allowed_msgs))
    end.

are_all_msgs_allowed([{all, all, all}]) ->
    true;
are_all_msgs_allowed(Allowed_msgs) ->
    false.

handle_msg([Node ,Sensor, Id, Time, Body], Config, true) ->
    lager:debug("got message : ~p : ~p", [Time, Body]),
    {driver, {Module, Func}, Module_config} = lists:keyfind(driver, 1, Config),
    Module:Func([Node ,Sensor, Id, Time, Body], Config, Module_config);

handle_msg([Node ,Sensor, Id, Time, Body], Config, false) ->
    lager:debug("got message which i don't understand : ~p", [{Node, Sensor, Id}]),
    Config.

driver_init(Module, false, Config) ->
    lager:debug("don't init driver : ~p", [Module]),
    check_init(Module);
driver_init(Module, true, Config) ->
    lager:debug("call init for driver : ~p", [Module]),    
    Module:init(Config).

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
    Name = proplists:get_value(name, Config),
    Id = sensor:get_id(Config),
    ets_mgr:init_table(self(), list_to_atom(Name ++ "_" ++ Id), Module_config).  

check_init(Module) ->
    Exports = proplists:get_value(exports, Module:module_info(), []),
    case proplists:get_value(init, Exports) of 
        undefined -> true;
        1 -> lager:warning("there is a init function in the module '~p', but it is false or not available in the config"),
             false;
        Any -> lager:waring("the init function has too many arguments"),
                false
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
    ?assertEqual(false, is_message_well_known({<<"test@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_3)).

-endif.