%% Copyright 2010 Ulf Angermann
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
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 16.11.2013
%%% -------------------------------------------------------------------
-module(cuberl).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0, stop/0]).
-export([connect/0, connect/2]).
-export([register_listener/1]).

start() ->
  	application:start(?MODULE).

stop() ->
	application:stop(?MODULE). 

connect() ->
    {ok, Ip} = application:get_env(?MODULE, ip),
    {ok, Port} = application:get_env(?MODULE, port),
    connect(Ip, Port).   

connect(Ip, Port) ->
    transceiver:connect(Ip, Port).

register_listener(Pid) when is_pid(Pid) ->
    cuberl_sender:set_listener(Pid). 
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
ensure_started(App) ->

    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
