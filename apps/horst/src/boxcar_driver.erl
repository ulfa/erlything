%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(boxcar_driver).

%% --------------------------------------------------------------------
%% defines
%% --------------------------------------------------------------------
-define(URI, "https://new.boxcar.io/api/notifications").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").


%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_msg/3]).

handle_msg([Node ,Sensor, Id, Time, [{account, Account}, {title, Title}, {message, Message}, {sound, Sound}]], Config, Module_config) ->
    send_message(Account, Title, Message, Sound),
    Config.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
send_message(Account, Title, Message, Sound) ->
    httpc:request(post, 
        {?URI,
        [],
        ?CONTENT_TYPE,
        mochiweb_util:urlencode([{"user_credentials", Account}, {"notification[title]",Title},{"notification[long_message]", Message}, {"notification[sound]", Sound}])
        },
        [{ssl, [{verify, 0}]}],
        [{verbose,verbose}]
        ).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
