%% Copyright 2010 Ulf
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
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(function_resource).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, to_html/2, to_json/2, content_types_provided/2, allowed_methods/2, resource_exists/2]).
-export([post_is_create/2, process_post/2, finish_request/2]).
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
init(Config) -> 
    %%{{trace, "/tmp"}, Config}.
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
    {['GET', 'POST'], ReqData, Context}.
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
    lager:info("1...... this is a test ....~p",[wrq:req_qs(ReqData)]),
    Body = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Button = get_value("button", Body),
    handle_post(Button, Body),
    {true, ReqData, Context}.

handle_post("save", Body) ->
    F_name = get_value("fName", Body),
    F_fun = get_value("fFun",Body),
    F_node =  get_value("fNode",Body),
    F_driver = get_value("fDriver",Body),
    F_id = get_value("fId",Body),
    F_comment = get_value("fComment", Body),
    F_message = {list_to_binary(F_node), list_to_binary(F_driver), list_to_binary(F_id)},
    Send_body = {save, F_name, F_message, F_fun, F_comment},
    lager:info("send the body : ~p",[Send_body]),
    sensor:send(?MODULE, Send_body); 
    
handle_post("run", Body) ->
    F_name = get_value("fName", Body),
    F_args = get_value("fArgs", Body),
    Msg = sender_util:create_message(node(), ?MODULE, "default", date:get_date_seconds(), {run, F_name, F_args }),    
    sender_util:send_message(Msg).
%
% This should return a list of pairs where each pair is of the form {Mediatype, Handler} 
% where Mediatype is a string of content-type format and the Handler is an atom naming 
% the function which can provide a resource representation in that media type. Content 
% negotiation is driven by this return value. For example, if a client request includes 
% an Accept header with a value that does not appear as a first element in any of the 
% return tuples, then a 406 Not Acceptable will be sent.
% 
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}, {"application/json", to_json}, {"application/x-www-form-urlencoded", process_form}] ,ReqData, Context}.
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
    lager:info("to_html : ~p",[wrq:req_qs(ReqData)]),
    Fun_name = wrq:get_qs_value("fun_name",ReqData),    
    {Name1, {F_node1, F_driver1, F_id1}, Command, Comment} = case get_fun(Node, Name, Fun_name) of 
        {N, {F_node, F_driver, F_id}, C, Co} -> {N, {F_node, F_driver, F_id}, C, Co};
        {N, [], C, Co} -> {N, {[], [], []}, C, Co}
    end,
    {ok, Content} = function_dtl:render([{node, Node},{fname, Name1}, {fnode, F_node1}, {fdriver, F_driver1}, {fid, F_id1},{command, Command}, {comment, Comment}]),
    {Content, ReqData, Context}.

to_json(ReqData, Context) ->
    {"not implemented yet", ReqData, Context}.

get_fun(Node, Name, Fun_name) when is_list(Node)->
    case rpc:call(list_to_atom(Node), thing, get_driver, [Name]) of 
        {badrpc, Reason} -> lager:error("got error during call ~p thing:get_driver(~p) with reason ~p", [Node, Name, Reason]),
                            [];
        {Driver, Config} -> Funs  = proplists:get_value(funs, Config, []),
                            get_command(Funs, Fun_name)

    end.

get_command(Funs, Name) ->
    case lists:keysearch(Name, 1, Funs) of
        {value, {N, M, F, C, Co}} -> {N, M, C, Co};
        false -> []
    end.

get_value(Key, List) ->
    proplists:get_value(Key, List). 
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.