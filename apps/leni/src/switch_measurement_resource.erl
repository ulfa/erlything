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

-module(switch_measurement_resource).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, to_html/2, content_types_provided/2, allowed_methods/2, resource_exists/2]).
-export([generate_etag/2]).
-export([is_authorized/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("webmachine/include/webmachine.hrl").
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {}).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
init(_Config) -> 
	{ok, #context{}}.
%
% Returning non-true values will result in 404 Not Found.
% 
resource_exists(ReqData, Context) ->
	{true, ReqData, Context}.

%
% true, if the service is available
%
service_available(ReqData, Context) ->
	{true, ReqData, Context}.

%
% If this returns anything other than true, the response will be 401 Unauthorized.
% The AuthHead return value will be used as the value in the WWW-Authenticate header.
%
is_authorized(ReqData, Context) ->
	login:login(ReqData, Context). 

forbidden(ReqData, Context) ->
	{false, ReqData, Context}.

%
% If the resource accepts POST requests to nonexistent resources, 
% then this should return true.
%
allow_missing_post(ReqData, Context) ->
	{false, ReqData, Context}.

malformed_request(ReqData, Context) ->
	{false, ReqData, Context}.

uri_too_long(ReqData, Context) ->
	{false, ReqData, Context}.

known_content_type(ReqData, Context) ->
	{true, ReqData, Context}.

valid_entity_length(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If the OPTIONS method is supported and is used, the return value of this 
% function is expected to be a list of pairs representing header names and 
% values that should appear in the response.
%
options(ReqData, Context) ->
	{[], ReqData, Context}.

valid_content_headers(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If a Method not in this list is requested, then a 405 Method Not Allowed
% will be sent. Note that these are all-caps and are atoms. (single-quoted)
%
allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.
%
% This is called when a DELETE request should be enacted 
% and should return true if the deletion succeeded.
%
delete_resource(ReqData, Context) ->
	{false, ReqData, Context}.
%
% This is only called after a successful delete_resource call, and should 
% return false if the deletion was accepted but cannot yet be guaranteed to have finished.
%
delete_completed(ReqData, Context) ->
	{true, ReqData, Context}.
%
% If POST requests should be treated as a request to put content into a 
% (potentially new) resource as opposed to being a generic submission for 
% processing, then this function should return true. If it does return true, 
% then create_path will be called and the rest of the request will be treated 
% much like a PUT to the Path entry returned by that call.
post_is_create(ReqData, Context) ->
	{false, ReqData, Context}.
%
% This will be called on a POST request if post_is_create returns true. 
% It is an error for this function to not produce a Path if post_is_create returns true. 
% The Path returned should be a valid URI part following the dispatcher prefix. 
% That Path will replace the previous one in the return value of wrq:disp_path(ReqData) 
% for all subsequent resource function calls in the course of this request.
%
create_path(ReqData, Context) ->
	{undefined, ReqData, Context}.
%
% If post_is_create returns false, then this will be called to process any POST requests. 
% If it succeeds, it should return true.
%
process_post(ReqData, Context) ->
	{false, ReqData, Context}.
%
% This should return a list of pairs where each pair is of the form {Mediatype, Handler} 
% where Mediatype is a string of content-type format and the Handler is an atom naming 
% the function which can provide a resource representation in that media type. Content 
% negotiation is driven by this return value. For example, if a client request includes 
% an Accept header with a value that does not appear as a first element in any of the 
% return tuples, then a 406 Not Acceptable will be sent.
% 
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}],ReqData, Context}.
%
% This is used similarly to content_types_provided, except that it is for incoming 
% resource representations -- for example, PUT requests. Handler functions usually 
% want to use wrq:req_body(ReqData) to access the incoming request body.
% 
content_types_accepted(ReqData, Context) ->
	{[], ReqData, Context}.
%
% If this is anything other than the atom no_charset, it must be a list of pairs where 
% each pair is of the form Charset, Converter where Charset is a string naming a charset
% and Converter is a callable function in the resource which will be called on the produced 
% body in a GET and ensure that it is in Charset.
%
charsets_provided(ReqData, Context) ->
	{no_charset, ReqData, Context}.
%
% This must be a list of pairs where in each pair Encoding is a string naming a valid 
% content encoding and Encoder is a callable function in the resource which will be called 
% on the produced body in a GET and ensure that it is so encoded. One useful setting is to 
% have the function check on method, and on GET requests return 
% [{"identity", fun(X) -> X end}, {"gzip", fun(X) -> zlib:gzip(X) end}] as this is all that 
% is needed to support gzip content encoding.
%
encodings_provided(ReqData, Context) ->
	{[{"identity", fun(X) -> X end}], ReqData, Context}.
%
% If this function is implemented, it should return a list of strings with header names that 
% should be included in a given response's Vary header. The standard conneg headers 
% (Accept, Accept-Encoding, Accept-Charset, Accept-Language) do not need to be specified 
% here as Webmachine will add the correct elements of those automatically depending on resource behavior.
%
variances(ReqData, Context) ->
	{[], ReqData, Context}.
%
% If this returns true, the client will receive a 409 Conflict.
%
is_conflict(ReqData, Context) ->
	{false, ReqData, Context}.
%
% If this returns true, then it is assumed that multiple representations of the response are possible 
% and a single one cannot be automatically chosen, so a 300 Multiple Choices will be sent instead of a 200.
% 
multiple_choices(ReqData, Context) ->
	{false, ReqData, Context}.


previously_existed(ReqData, Context) ->
	{false, ReqData, Context}.
%
% {true, MovedURI} | false
%
moved_permanently(ReqData, Context) ->
	{false, ReqData, Context}.
%
% {true, MovedURI} | false
%
moved_temporarily(ReqData, Context) ->
	{false, ReqData, Context}.
%
% undefined | YYYY,MM,DD, Hour,Min,Sec
%
last_modified(ReqData, Context) ->
	{undefined, ReqData, Context}.
%
% undefined | YYYY,MM,DD, Hour,Min,Sec 
%
expires(ReqData, Context) ->
	{undefined, ReqData, Context}.
%
% If this returns a value, it will be used as the value of the ETag header 
% and for comparison in conditional requests.
%
generate_etag(ReqData, Context) ->
	{undefined, ReqData, Context}.
% This function, if exported, is called just before the final response is constructed and sent. 
% The Result is ignored, so any effect of this function must be by returning a modified ReqData 
%
finish_request(ReqData, Context) ->
	{true, ReqData, Context}.
%% --------------------------------------------------------------------
%%% Additional functions
%% --------------------------------------------------------------------
to_html(ReqData, Context) ->
    Node = wrq:get_qs_value("node",ReqData),
    Name = wrq:get_qs_value("name",ReqData),
	{ok, Content} = switch_measurement_dtl:render([{measurements,  get_data(Node, Name)}]),
    {Content, ReqData, Context}. 

get_data(Node, Name) when is_list(Node)->
    case rpc:call(list_to_atom(Node), thing, get_module_config, [Name]) of 
        {badrpc, Reason} -> lager:error("got error during call ~p thing:get_driver(~p) with reason ~p", [Node, Name, Reason]),
                            [];
          [{data, Data}] -> process_data(Data, []) 
    end.

process_data([], Acc) ->
    Acc;
process_data([{Device, Name, Data}|Tail], Acc) ->
    List_1 = insert_status_null(Data, Data),
    List_2 = calc_period(List_1, []),
    process_data(Tail, [{Device, Name, List_2}|Acc]).
insert_status_null([{Time, "1"}|Tail], List) ->
    [{date:get_date_seconds(), "0"}|List];
insert_status_null([{Time, "0"}|Tail], List) ->
    List.

calc_period([], Acc) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, Acc);
calc_period([{T1, "0"}, {T2, "1"}|List], Acc) ->
	calc_period(List, [T1 - T2|Acc]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
%% can't test it yet, because of the dynamic timestamp
insert_status_null_test() ->
    ?assertEqual([{63549225463, "0"}, {1234, "1"}, {345, "0"}, {345444, "1"}], insert_status_null([{1234, "1"}, {345, "0"}, {345444, "1"}], [{1234, "1"}, {345, "0"}, {345444, "1"}])).

calc_period_test() ->
	?assertEqual([2], calc_period([{4, "0"},{2, "1"}], [])).

process_data_test() ->
List = [{"11111 1","Ventilator",
         [{63549232701,"0"},{63549232694,"1"}]},
        {"11111 2","Licht",
         [{63549232683,"1"},{63549232681,"0"},{63549232670,"1"}]}],
    ?assertEqual([{"11111 2","Licht",2865}, {"11111 1","Ventilator",7}],process_data(List, [])).
-endif.
