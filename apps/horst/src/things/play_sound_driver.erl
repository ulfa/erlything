%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(play_sound_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, Optional, ?ON] = Msg, Config, Module_config) ->
    Sound = config:get_value(sound, Module_config), 
    play_sound(Sound),
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
play_sound(Sound) ->
    lager:info("let's play : ~p ", [Sound]),
    play_sound(os:type(), Sound).

    play_sound({unix,linux}, Sound) ->
    os:cmd("mpg123 " ++ filename:append(code:priv_dir(horst), Sound));
play_sound({unix, darwin}, Sound) ->
    os:cmd("afplay " ++ filename:append(code:priv_dir(horst), Sound));
play_sound({A, B}, _Sound) ->
    lager:error("I don't know this system. Please, inform the author ~p", [{A,B}]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
