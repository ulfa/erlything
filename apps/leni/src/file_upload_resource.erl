-module(file_upload_resource).

-export([init/1, to_html/2, allowed_methods/2, process_post/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(context, {path=[]}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
init(Config) ->
    Path = proplists:get_value(path, Config, []),  
    {ok, #context{path=Path}}.

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

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context=#context{path=Path}) ->
    CT =wrq:get_req_header("content-type", ReqData),
    ContentType = wrq:get_req_header("content-type", ReqData),
    Boundary = string:substr(ContentType, string:str(ContentType, "boundary=") + length("boundary=")),
    {FileName, FileSize, Content} = get_streamed_body(webmachine_multipart:stream_parts(wrq:stream_req_body(ReqData, 1024), Boundary), [],[]),
    StorePath = filename:join([Path, FileName]),
    filelib:ensure_dir(StorePath),
    file:write_file(StorePath, Content),
    {ok, Ctxt} = file_upload_complete_dtl:render([{filesize, FileSize}, {filename, StorePath}]),
    NewRD1 = wrq:append_to_response_body(Ctxt, ReqData),
    {true, NewRD1, Context}.

to_html(ReqData, Context) ->
    {ok, Content} = file_upload_dtl:render([]),
    {Content, ReqData, Content}.

get_streamed_body(done_parts, FileName, Acc) ->
    Bin = iolist_to_binary(lists:reverse(Acc)),
    {FileName, size(Bin)/1024.0, Bin};
get_streamed_body({{"filedata", {Params, _Hdrs}, Content}, Next}, Props, Acc) ->
    FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
    get_streamed_body(Next(),[FileName|Props],[Content|Acc]).
