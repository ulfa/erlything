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
%%% Created : 
%%% -------------------------------------------------------------------
-module(erlything).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0, stop/0]).


start() ->
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(goldrush),
    ensure_started(lager),
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(inets), 
    ensure_started(xmerl),
    start_mnesia(),
    ensure_started(roni),
    ensure_started(sue),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(gpio),
    ensure_started(horst),
    ensure_started(leni),
    ensure_started(moni),
    application:start(?MODULE).

stop() ->
    ok.
%%    ensure_stopped(sue),
%%    ensure_stopped(leni),
%%    ensure_stopped(moni),
%%    ensure_stopped(roni),
%%    ensure_stopped(gpio), 
%%    ensure_stopped(horst).
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

ensure_stopped(App) ->
    case application:stop(App) of
        ok ->
            ok;
        {error, Reason} ->
            ok
    end.

start_mnesia() ->
    lager:info("create the schema for the database"),
    mnesia:create_schema([node()]),
    lager:info("start the database"),
    ensure_started(mnesia).

stop_mnesia() ->
    mnesia:stop().

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
