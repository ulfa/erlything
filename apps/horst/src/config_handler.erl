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
-export([get_config/2, create_child_specs/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
get_config(Application, Config_file) ->
	{ok, Config} = file:consult(filename:join([code:priv_dir(Application),"config", Config_file])),
	Config.

create_child_specs(Type_in, Config) ->
	[?CHILD(Name, worker) || {Name, Type, Activ} <- Config, is_activ(Activ), is_type(Type, Type_in)].

is_activ(true) ->
	true;
is_activ(false) ->
	false.

is_type(Type, Type) ->
	true;
is_type(Type, Type_1) ->
	false.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_config_test() ->
	application:load(horst),
	Config = get_config(horst, ?CONFIG_FILE).
	%%?assertEqual([{hc_sr501_sensor}, {dht22_sensor}], Config).

start_child_test() ->
	application:load(horst),
	Config = [{hc_sr501_sensor, sensor, true},
			  {dht22_sensor, sensor, false},
 			  {dht22_actor, actor, true}],	
	?assertEqual([?CHILD(hc_sr501_sensor, worker)], create_child_specs(sensor, Config)).	
-endif.
