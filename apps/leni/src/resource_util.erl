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

-module(resource_util).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([convert_nodes/1]).

convert_nodes(Nodes) ->
    [{Node, convert_date_in_things(Things)}|| {Node, Things} <- Nodes].

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
convert_date_in_things(Things) ->
    lists:foldl(fun({Pid, Name, Date, Description, Driver, Icon}, Acc) -> [{Pid, Name, date:get_formated_date_for_now(Date), Description, Driver, Icon}|Acc] end, [], Things).  

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
convert_now_to_date_test() ->
    Things = [{"<0.94.0>",'Temperatur_Anzeige',
                       {1379,236269,790482},
                       "Temp sensor in my office",dht22_display_driver},
                      {"<0.95.0>",'Message_Logger',
                       {1379,236269,790612},
                       "Stores the last 20 messages and counts all messages",
                       message_counter_driver}],
    convert_date_in_things(Things).
-endif.
