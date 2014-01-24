%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(moni).

%% Application callbacks
-export([start/0, stop/0]).

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
	ensure_started(webmachine),
	application:start(?MODULE).

stop() ->
    application:stop(?MODULE).
