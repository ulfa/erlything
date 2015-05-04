-module(horst_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
	start_database(),
    horst_sup:start_link().

stop(_State) ->
    ok.

start_database() ->
    lager:info("create the schema for the database"),
    application:stop(mnesia),    
    mnesia:create_schema([node()]),
    lager:info("start the database"),
    application:start(mnesia).