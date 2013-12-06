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

-module(leni).

%% Application callbacks
-export([start/0, stop/0]).
-export([set_debug/0, set_info/0]).

ensure_started(App) ->

    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
	
start() ->
	ensure_started(inets), 
	ensure_started(crypto),
  	ensure_started(lager),
	ensure_started(mochiweb),
	%%application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
	ensure_started(webmachine),
	ensure_started(sue),	
	application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

set_debug() ->
    lager:set_loglevel(lager_console_backend, debug).

set_info() ->
    lager:set_loglevel(lager_console_backend, info).
