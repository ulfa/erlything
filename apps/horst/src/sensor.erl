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
-export([create_message/4]).
-export([send/3]).
-export([send_message/2]).
-export([send_messages/2]).
-export([send_after/4]).
-export([generate_messages/1, generate_messages/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
create_message(Config, Module, Body) when is_list(Module) ->
	create_message(Config, list_to_atom(Module), Body);
create_message(Config, Module, Body) ->
	create_message(node(), Module, config_handler:get_id(Config), Body).

create_message(Node, Module, Id, Body) ->
	create_message(Node, Module, Id, date:get_date_seconds(), Body).

create_message(Node, Module, Id, Time, Body) when is_list(Module) ->
	create_message(Node, list_to_atom(Module), Id, Time, Body);

create_message(Node, Module, Id, Time, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Module, utf8), list_to_binary(Id), list_to_binary(integer_to_list(Time)), Body].

%%create_message(Config, Node, Module, Body) ->
%%	create_message(Node, Module, config_handler:get_id(Config), date:get_date_seconds(), Body).

%%create_message(Node, Module, Body) ->
%%	create_message(Node, Module, config_handler:get_id([]), date:get_date_seconds(), Body).


%% doc sends a list of messages
-spec send_messages(_Config, [node()]) -> ok.
send_messages(_Config, []) ->
	ok;
send_messages(Config, [{Name, Message}|Messages]) when is_list(Messages) ->
	send(Config, Name, Message),
	send_messages(Config, Messages).

send(Config, Module, Body) ->	
	send_message(create_message(Config, Module, Body)).

send_message(Message) ->
	send_message(nodes(), Message).

send_message(Node, Message) when is_atom(Node) ->
	send_message([Node], Message);

send_message(Nodes, Message) ->
	send_message(Nodes, 'actor_group', Message).    

send_message(Nodes, Target, Message) ->
	send_message(node(), Nodes, Target, Message).

send_message(Node, Nodes, Target, [Node_1, Module, Id, Time, Body] = Message) ->
	rpc:abcast([Node|Nodes], Target, Message),
	ok.

send_after(Pid, Name, Time, Body) when is_list(Body) ->
	erlang:send_after(Time, Pid, {send_after, Name, Body}).    
%%
%% doc generates messages and send them. !!! only for testing !!!
%%
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

send_message_test() ->
	meck:new(rpc, [unstick]),
	meck:expect(rpc, abcast, fun(['nonode@nohost'], 'actor_group', "This is a test") -> abcast end),
	?assertEqual(ok, send_message("This is a test")),
	?assert(meck:validate(rpc)).

create_message_test() ->
    ?assertEqual([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"1">>, <<"63540684780">>,"FALLING"], create_message('horst@notebook', "hc_sr501_sensor", "1", 63540684780, "FALLING")),
    ?assertMatch([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"default">>, A,"FALLING"], [<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"default">>, A,"FALLING"] = create_message('horst@notebook', 'hc_sr501_sensor',  "default", "FALLING")),
    ?assertMatch([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"1">>, A,"FALLING"], [<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"1">>, A,"FALLING"] = create_message('horst@notebook', 'hc_sr501_sensor', "1",  "FALLING")),
    ?assertMatch([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"default">>, A,"FALLING"], [<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"default">>, A,"FALLING"] = create_message('horst@notebook', 'hc_sr501_sensor', "default" ,"FALLING")).


-endif.