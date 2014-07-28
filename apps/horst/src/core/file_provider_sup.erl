%% Copyright 2012 Ulf Angermann
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
%%% Author  : Ulf Angermann 
%%% Description :
%%%
%%% Created : 08.11.2013
%%% -------------------------------------------------------------------
-module(file_provider_sup).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start_child/0]).
-export([init/1]).
-export([get_pic/1, get_log/1]).
-define(LHS(),{file_provider, {file_provider, start_link, []}, transient, brutal_kill, worker, [file_provider]}).
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(file_provider_sup, []).

get_pic(Name) ->
	get_file(get_pic, Name).
get_log(Name) ->
	get_file(get_log, Name).
get_file(File_type, Name) ->
    {ok, Pid} = start_child(),
    File = gen_server:call(Pid, {File_type, Name}),
    gen_server:call(Pid, stop),
    File.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->     
    RestartStrategy = {simple_one_for_one, 1, 3600},
    {ok, {RestartStrategy, [?LHS()]}}.
