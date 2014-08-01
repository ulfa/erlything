%%
%% Copyright (c) 2013 Ulf Angermann  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(sender_util).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_message/5, create_message/4, create_message/6, create_message/3, create_message/2]).
-export([send_message/1, send_message/2]).
-export([encode/1]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
create_message(Module, Body) ->
  create_message(node(), Module, get_id([]), get_name(), Body).

create_message(Node, Module, Body) ->
  create_message(Node, Module, get_id([]), get_name(), Body).

create_message(Node, Module, Id, Body) ->
  create_message(Node, Module, Id, get_name(), Body).

create_message(Node, Module, Id, Optional, Body) ->
  create_message(Node, Module, Id, date:get_date_seconds(), Optional, Body).

create_message(Node, Sensor, Id, Time, Optional, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Sensor, utf8), list_to_binary(Id), list_to_binary(integer_to_list(Time)), Optional, Body].

send_message(Message) ->
  send_message(nodes(), Message).

send_message(Node, [Node_1, Module, Id, Time, Body] = Message) when is_atom(Node)->
  rpc:abcast([Node], 'actor_group', Message);

send_message(Nodes, Message) ->
  send_message(Nodes, 'actor_group', Message).    

send_message(Nodes, Target, [Node_1, Module, Id, Time, Optional, Body] = Message) when is_list(Nodes)->
  rpc:abcast([node()|Nodes], Target, Message).    

encode([]) ->
	[];
encode(List) ->
	[{pid_to_list(Pid), atom_to_list(Module), Description} || {Pid, Module, Description} <- List].

get_result({}) ->
	[].

get_id([]) ->
  "default";
get_id(Config) when is_list(Config) ->
  proplists:get_value(id, Config, "default"). 

get_name() ->
  erlang:process_info(self(), registered_name).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
encode_test() ->
	A= [{list_to_pid("<11802.80.0>"),seven_eleven_actor,"Actor, which rings the 7-11 bell"},
   		{list_to_pid("<11802.81.0>"),dht22_actor,"Actor, which is responspable for dht22 messages"}],
	B= [{"<11802.80.0>","seven_eleven_actor","Actor, which rings the 7-11 bell"},
   		{"<11802.81.0>","dht22_actor","Actor, which is responspable for dht22 messages"}],
   	?assertEqual(B, encode(A)).


get_result_test() ->

 A= [
  	{badrpc,{'EXIT',{undef,[{ronja,get_actors,[],[]},{rpc,'-handle_call_call/6-fun-0-',5,[{file,"rpc.erl"},{line,203}]}]}}},
  	{badrpc,{'EXIT',{undef,[{ronja,get_actors,[],[]},{rpc,'-handle_call_call/6-fun-0-',5,[{file,"rpc.erl"},{line,203}]}]}}},
  	[
  		{"<11876.80.0>",seven_eleven_actor,"Actor, which rings the 7-11 bell"},
   		{"<11876.81.0>",dht22_actor,"Actor, which is responspable for dht22 messages"}
  	]
  ],

  [R || R <- A, is_list(R) ].
-endif.
