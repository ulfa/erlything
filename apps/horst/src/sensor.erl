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
-export([create_message/5, send_message/2]).
-export([get_description/0, get_type/0]).
-export([get_seconds/0]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
create_message(Node, Sensor, Id, Time, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Sensor, utf8), list_to_binary(Id), list_to_binary(integer_to_list(Time)), Body].

send_message(Nodes, Message) ->
	send_message(Nodes, 'actor_group', Message).    
send_message(Nodes, Target, Message) ->
	rpc:abcast(Nodes, Target, Message).    

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_description() ->
	"short description".
get_type() ->
    sensor.
get_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
     
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