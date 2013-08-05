%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(config_handler).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_config/2, create_child_specs/2, create_thing_spec/1]).
-export([get_messages_for_module/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
get_messages_for_module(Modul, Id) ->
	Messages = get_config(horst, ?MESSAGES_CONFIG),
	case lists:keyfind(Modul, 1, Messages) of 
		false ->  sets:new();
		{M, MSGs} -> sets:from_list(MSGs)
	end. 

get_config(Application, Config_file) ->
	{ok, Config} = file:consult(filename:join([code:priv_dir(Application),"config", Config_file])),
	Config.

create_child_specs(Type_in, Config) ->
	[?CHILD(Name, worker) || {Name, Type, Activ} <- Config, is_activ(Activ), is_type(Type, Type_in)].

is_activ(true) ->
	true;
is_activ(false) ->
	false;
is_activ(List) when is_list(List) ->
	proplists:get_value(activ, List, false). 

is_type(Type, Type) ->
	true;
is_type(Type, Type_1) ->
	false.

create_thing_spec(Config) ->
	[?THING(list_to_atom(Name), [{name, Name}|List]) || {thing, Name, List} <- Config, is_activ(List)].
	
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

create_thing_spec_test() ->
	Config = [{thing, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]}],
	P = [{name, "Switches office"},{type, actor}, {driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]}, {activ, true},{description,"Switches in my office"}],
	io:format("...~p~n",[create_thing_spec(Config)]),
	?assertEqual([{'Switches office', {thing, start_link, [P]}, permanent, 5000, worker, [thing]}], create_thing_spec(Config)).

get_config_test() ->
	application:load(horst),
	Config = get_config(horst, ?SUPERVISOR_CONFIG).
	%%?assertEqual([{hc_sr501_sensor}, {dht22_sensor}], Config).

start_child_test() ->
	application:load(horst),
	Config = [{hc_sr501_sensor, sensor, true},
			  {dht22_sensor, sensor, false},
 			  {dht22_actor, actor, true}],	
	?assertEqual([?CHILD(hc_sr501_sensor, worker)], create_child_specs(sensor, Config)).	

get_messages_for_module_test() ->
	?assertEqual(sets:from_list([{<<"horst@ronja">>,<<"hc_sr501_sensor">>, <<"0">>}]), get_messages_for_module(seven_eleven_actor, "0")).
-endif.
