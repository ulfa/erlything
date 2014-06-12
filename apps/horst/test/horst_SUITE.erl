%% Copyright 2014 Ulf Angermann
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
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(horst_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-define(SLEEP, 1000).
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [{config_driver, [sequence], [copy_config, delete_config]},
             {node_config, [sequence], [set_things_file, set_active, delete_thing_config, get_things_config, add_thing_to_config, set_messages_file]}
            ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() -> [{group, node_config},
          {group, config_driver}
         ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) -> 
    ct:log("nodes : ~p~n", [ct:get_config(nodes, Config)]),
    [pong = net_adm:ping(Node) || Node <- ct:get_config(nodes, Config)], 
	Config.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->    
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
%%-----------------------------node_config--------------------------
init_per_testcase(set_things_file, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", []]),
  ct:sleep(?SLEEP), 
  Config;

init_per_testcase(set_active, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", [get_thing_data()]]),
  ct:sleep(?SLEEP),
  Config;

init_per_testcase(delete_thing_config, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", [get_thing_data()]]),
  ct:sleep(?SLEEP),
  Config;

init_per_testcase(get_things_config, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", [get_thing_data()]]),
  ct:sleep(?SLEEP),
  Config;

init_per_testcase(add_thing_to_config, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", []]),
  ct:sleep(?SLEEP),
  Config;

init_per_testcase(set_messages_file, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_messages_file, ["messages.config", []]),
  ok = rpc:call(ct:get_config(node, Config), node_config, set_messages_file, ["messages.config", []]),
  ct:sleep(?SLEEP),
  Config;
%%-----------------------------config_driver--------------------------
init_per_testcase(copy_config, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", [get_config_driver(),get_thing_data()]]),  
  ct:sleep(?SLEEP),
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_active, ["Config_Manager", true]),
  ok = rpc:call(ct:get_config(node, Config), node_config, set_things_file, ["things.config", []]),

  Message = {{config_driver,"default"}, [{all, <<"horst_SUITE">>, all}]},
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_messages_file, ["messages.config", [Message]]),
  ct:sleep(?SLEEP),
  Config;
%%-----------------------------mnesia_driver--------------------------
init_per_testcase(handle_msg, Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_things_file, ["things.config", [get_mnesia_driver()]]),
  ct:sleep(?SLEEP),

  Message = {{mnesia_driver, "default"}, [{all, <<"horst_SUITE">>, all}]},
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_messages_file, ["messages.config", [Message]]),
  ct:sleep(?SLEEP),
  Table_name = mnesia_driver:create_table_name("test@node", "module", "default"), 
  mnesia:delete_table(Table_name), 
  Config;

init_per_testcase(_TestCase, Config) ->
  Config.
%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%%  Here are the test cases
%%--------------------------------------------------------------------
%%-----------------------------node_config--------------------------
set_things_file(Config) ->
    ok = rpc:call(ct:get_config(node, Config), node_config, set_things_file, ["things.config", [get_thing_data()]]).

set_active(Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_active, ["Sample_Sensor1", true]),
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_active, ["Sample_Sensor1", false]),
  {error, _Reason} =  rpc:call('erlything1@macbook-pro', node_config, set_active, ["Sample_Sensor1", "something"]).

delete_thing_config(Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, delete_thing_config, ["Sample_Sensor1"]).

get_things_config(Config) ->
  Node = ct:get_config(node_1, Config),
  {Node,[{thing, "Sample_Sensor1", _List}]} =  rpc:call(ct:get_config(node_1, Config), node_config, get_things_config,[]).

add_thing_to_config(Config) ->
  ok = rpc:call(ct:get_config(node_1, Config), node_config, add_thing_to_config, [get_thing_data(), "things.config"]).

set_messages_file(Config) ->
  Message = {{config_driver,"default"}, [{all, <<"horst_SUITE">>, all}]},
  ok = rpc:call(ct:get_config(node_1, Config), node_config, set_messages_file, ["messages.config", [Message]]).

%%-----------------------------config_driver--------------------------
copy_config(Config) ->
  Message = create_message(node(), 'horst_SUITE', "default", [{action, "copy"}, {thing, "Sample_Sensor1"}, {target, atom_to_list(ct:get_config(node, Config))}]),
  sensor:send_message(ct:get_config(node_1, Config), Message),
  ct:sleep(?SLEEP),
  Node = ct:get_config(node, Config),
  {Node,[{thing, "Sample_Sensor1", _List}]} =  rpc:call(ct:get_config(node, Config), node_config, get_things_config,[]).

delete_config(_Config) ->
  ok.

%%-----------------------------mnesia_driver--------------------------
handle_msg(Config) ->
  mnesia_driver:handle_msg([<<"test@node">>, <<"module">>, <<"default">>, <<"12345678">>, {test, value}], [], []).

%%--------------------------------------------------------------------
%%  only helper
%%--------------------------------------------------------------------
get_thing_data() ->
  {thing,"Sample_Sensor1",
     [{type,sensor},
      {ets,true},
      {icon,"temp.png"},
      {id, "default"},
      {driver,{sample_driver,call_sensor},[{init,true},{data,[]}]},
      {activ,false},
      {timer,5000},
      {database,[]},
      {description,"Sample sensor for playing with"}]}.
get_config_driver() ->
  {thing,"Config_Manager",
     [{type,actor},
      {ets,false},
      {icon,"temp.png"},
      {driver,{config_driver,handle_msg},[{data,[]}]},
      {activ,false},
      {timer,0},
      {database,[]},
      {description,"Things Config Manager"}]}.

get_mnesia_driver() ->
  {thing,"Config_Manager",
     [{type,actor},
      {ets,false},
      {icon,"temp.png"},
      {driver,{mnesia_driver,handle_msg},[{data,[]}]},
      {activ,true},
      {timer,0},
      {database,[]},
      {description,"The database actor"}]}.


create_message(Node, Module, Id, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Module, utf8), list_to_binary(Id), list_to_binary(integer_to_list(get_date_seconds())), Body].

get_date_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).


