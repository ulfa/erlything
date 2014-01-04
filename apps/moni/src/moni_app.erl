%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(moni_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start(_StartType, _StartArgs) ->
    ensure_started(inets), 
    ensure_started(crypto),    
    ensure_started(mochiweb),   
    ensure_started(webmachine),
  	moni_sup:start_link().

stop(_State) ->
    ok.
