%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(funrunner_driver).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1]).
-export([handle_msg/3]).
-export([test_me/0, test_me_1/0, test_exception/0]).

init(Config) ->
    lager:info("funrunner_driver:init('~p')", [Config]),
    Config.

handle_msg([Node ,Sensor, Id, Time, {save, Name, Command, Comment}], Config, Module_config) ->
    lager:info("save fun under name : ~p with command : ~p and comment : ~p ", [Name, Command, Comment]),
    Funs = proplists:get_value(funs, Module_config, []),
    try 
        {ok, Fun} = command_to_fun(Command),
        create_message_and_send(Module_config, {save_result, Name, ok}),
        Module_config_1 = lists:keyreplace(funs, 1 , Module_config, {funs, lists:keystore(Name, 1, Funs, {Name, Fun, Command, Comment})}),    
        lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1})
    catch
        _:Error -> create_message_and_send(Module_config, {error, "saving fun with name : " ++ Name ++ " command : " ++ Command, Error}),
                   Module_config_2 = lists:keyreplace(errors, 1 , Module_config, {errors, [{Name, "saving command : " ++ Command, Error}]}),    
                   lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_2})
    end;

handle_msg([Node ,Sensor, Id, Time, {run, Name, Args}], Config, Module_config) ->
    lager:info("run fun with name : ~p and arguments : ~p", [Name, Args]),
    Funs = proplists:get_value(funs, Module_config, []),
    Fun = get_fun(Funs, Name),
    try 
        Arguments  = string_to_args("[" ++ Args ++ "]."),
        Result = run_fun(Fun, args_to_types(Arguments)),
        lager:info("Result for fun with name : ~p is : ~p", [Name, Result]),
        create_message_and_send(Module_config, Name,{run_result, Name, {ok,Result}})
    catch 
        _:Error -> create_message_and_send(Module_config, {error, "running fun with name : " ++ Name ++ " and args : ~p " ++ Args ++ " ", Error})
    end,
    Config;

handle_msg([Node ,Sensor, Id, Time, {list, Name}], Config, Module_config) ->
    lager:info("list the fun with name : ~p", [Name]),
    Funs = proplists:get_value(funs, Module_config, []),
    Result = get_command(Funs, Name),
    create_message_and_send(Module_config, {list_result, Name, {ok, Result}}),
    Config;

handle_msg([Node ,Sensor, Id, Time, {list}], Config, Module_config) ->
    lager:info("list all funs "),
    Funs = proplists:get_value(funs, Module_config, []),
    Result = get_commands(Funs),
    create_message_and_send(Module_config, {list_result, {ok, Result}}),
    Config;

handle_msg([Node ,Sensor, Id, Time, {error, Text, Others}], Config, Module_config) ->
    lager:error("An error occured: ~p : ~p", [Text, Others]),
%%    Module_config_1 = lists:keyreplace(errors, 1, Module_config, {errors, [{Text, Others}]}),
%%    lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1}).
    Config;

handle_msg([Node ,Sensor, Id, Time, Other], Config, Module_config) ->
    lager:warning("funrunner_driver got an unkown message : ~p", [Other]),  
    Config.
stop(Config) ->
    lager:info("funrunner_driver:stop('~p')", [Config]),  
    Config.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

create_message_and_send(Module_config, Result) ->
    create_message_and_send(Module_config, sensor:get_id(Module_config), Result).

create_message_and_send(Module_config, Id, Result) ->
    Message = sensor:create_message(node(), ?MODULE, Id, Result),
    sensor:send_message(nodes(), Message). 


command_to_fun(Command) ->
    {ok, Tokens,_} = erl_scan:string(Command),
    {ok, Fun} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Fun, []),
    {ok, Value}.

string_to_args(Args) ->
    {ok, Tokens,_} = erl_scan:string(Args),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.    

args_to_types(Args) ->
    [convert(Value, Type) || {Value, Type} <- Args].

convert(Value, int) when is_integer(Value) ->
    Value;
convert(Value, int)  ->
    list_to_integer(Value).


run_fun(Fun, Args) when is_function(Fun) ->
    Fun(Args).      

get_fun(Funs, Name) ->  
    case lists:keysearch(Name, 1, Funs) of
        {value, {N, F, C, Co}} -> F;
        false -> []
    end.
get_commands(Funs) ->
    [{N, C, Co} || {N, F, C, Co} <- Funs].  
get_command(Funs, Name) ->
    case lists:keysearch(Name, 1, Funs) of
        {value, {N, F, C, Co}} -> {N, C, Co};
        false -> []
    end.
    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
test_me() ->
    Message = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "arg_test", "fun([X]) -> X + 1 end.", "Das ist ein Argument Test"}), 
    sensor:send_message(nodes(), Message),
    Message1 = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {list}), 
    sensor:send_message(nodes(), Message1),
    Message2 = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {run, "arg_test", "{1, int}"}), 
    sensor:send_message(nodes(), Message2).

test_me_1() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "send_test", "fun([Args]) -> M=sensor:create_message(node(), 'funrunner', {'funrunner test'}), sensor:send_message(nodes(), M)  end.", "Das ist ein Argument Test"}),
    sensor:send_message(nodes(), Message),
    Message1=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {list}), 
    sensor:send_message(nodes(), Message1),
    Message2=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {run, "send_test", []}), 
    sensor:send_message(nodes(), Message2).

test_exception() ->
    try  
        command_to_fun("fun(X) -> X + 1 end")
    catch
        _:Error -> lager:info("Error : ~p", [Error])
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
string_to_args_test() ->
    ?assertEqual([{1,i}, {2,s}],string_to_args("[{1,i}, {2,s}].")).

args_to_types_test() ->
    ?assertEqual([1],args_to_types([{1,int}])).


-endif.