%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(sensor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_message/5, create_message/3, send_message/2]).
-export([get_id/1]).
-export([generate_messages/1, generate_messages/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------

create_message(Node, Sensor, Body) ->
	create_message(Node, Sensor, "0", date:get_date_seconds(), Body).
create_message(Node, Sensor, Id, Time, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Sensor, utf8), list_to_binary(Id), list_to_binary(integer_to_list(Time)), Body].

send_message(Nodes, Message) ->
	send_message(Nodes, 'actor_group', Message).    
send_message(Nodes, Target, Message) ->
	rpc:abcast([node()|Nodes], Target, Message).    

get_id(Config) when is_list(Config) ->
	proplists:get_value(id, Config, "0"). 

generate_messages(Loop_count) ->
	timer:tc(sensor, generate_messages, [Loop_count, 0]).
generate_messages(Counter, Counter) ->
	lager:info("finished generate_messages. ~p were generated.", [Counter]);
generate_messages(Counter, Value) ->
	Msg = create_message(node(), ?MODULE, {test_message, Value}),
	send_message(nodes(), Msg),
	generate_messages(Counter, Value + 1).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

create_message_test() ->
    ?assertEqual([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"1">>, <<"63540684780">>,"FALLING"], create_message('horst@notebook', 'hc_sr501_sensor', "1", 63540684780, "FALLING")).

send_message_test() ->
	?debugFmt("... : ~p", [node()]),
	dummy_actor:start(),
	Msg = create_message('nonode@nohost', 'hc_sr501_sensor', "1", 63540684780, [{temp, "10.0"}, {hum, "10%"}]),
	?debugFmt("... : ~p", [Msg]),
	send_message(['nonode@nohost'], 'dummy_actor', Msg).
-endif.