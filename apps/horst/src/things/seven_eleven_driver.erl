%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(seven_eleven_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, ?ON] = Msg, Config, Module_config) ->
    play_sound(),
    Config;
%%
%% This function handles unknwon messages.
%%
handle_msg(Unknown_message, Config, Module_config) ->
    lager:warning("~p got an unkown message with values: ~p",[?MODULE, Unknown_message]),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
play_sound() ->
    lager:info("let's ring the bell!"),
    play_sound(os:type()).

play_sound({unix,linux}) ->
	os:cmd("mpg123 " ++ filename:append(code:priv_dir(horst), "sounds/7-11.mp3"));
play_sound({unix, darwin}) ->
    os:cmd("afplay " ++ filename:append(code:priv_dir(horst), "sounds/7-11.mp3"));
play_sound({A, B}) ->
    lager:error("I don't know this system. Please, inform the author ~p", [{A,B}]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
