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

-module(login).

-export([login/2]).

-include_lib("webmachine/include/webmachine.hrl").

login(ReqData, Context) ->
    case is_peer_allowed(ReqData) of 
        true -> lager:debug("peer is allowed"),
                {true, ReqData, Context};
        false -> case wrq:get_req_header("authorization", ReqData) of
		  "Basic " ++ Base64 -> Str = base64:mime_decode_to_string(Base64),
			[Account, Password] = string:tokens(Str, ":"),
			case account:is_valid_account(Account, Password) of
				true -> lager:debug("sucessful login"),
						{true, ReqData, Context};
                false-> lager:error("Account : ~p, password: ~p  wrong", [Account, Password]),
                		{"Basic realm=Webmachine", ReqData, Context}
             end;
                		_ -> {"Basic realm=Webmachine", ReqData, Context}
	   end
    end.

is_peer_allowed(ReqData) ->
    Peer = wrq:peer(ReqData),
    {ok, Peers} = application:get_env(leni, peers), 
    lager:debug("Peer : ~p ~p", [Peer, Peers]),
    lists:member(Peer, Peers).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.