%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(moni_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = get_ip(),
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, get_port()},
                 {dispatch, Dispatch},
                 {dispatch_group, moni},
                 {name, moni}],
    Web = {wm_instance_1, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.

get_port() ->
    {ok, Port} = application:get_env(moni, port),
    Port.

get_ip() ->
    case application:get_env(moni, ip) of 
        undefined ->  "0.0.0.0";
        {ok, Ip} -> Ip
    end.