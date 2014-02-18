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
-export([test_me/0, test_me_1/0, test_me_2/0, test_me_3/0, test_me_4/0,test_exception/0]).
-export([test_schimmel/0, test_schimmel_data/0]).

init(Config) ->
    lager:info("funrunner_driver:init('~p')", [Config]),
    Config.

handle_msg([Node ,Sensor, Id, Time, {save, Name, Message, Command, Comment}], Config, Module_config) ->
    save_fun(Name, Message, Command, Comment, Config, Module_config);

handle_msg([Node ,Sensor, Id, Time, {run,{Node_1, Driver_1, Id_1, Time_1, Body}}], Config, Module_config) ->
    Msg = {Node_1, Driver_1, Id_1, Body},
    lager:info("run fun for message : ~p ", [Msg]),
    Funs = proplists:get_value(funs, Module_config, []),
    {Name, Fun} = get_fun(Funs, {Node_1, Driver_1, Id_1}),      
    try
        Result = run_fun(Fun, Name, Body),
        lager:info("Result of fun Message : ~p is : ~p", [Name, Result]),
        create_message_and_send(Module_config, {run_result, Name, {ok,Result}})
    catch   
        _:Error -> create_message_and_send(Module_config, {error, "running fun with name : " ++ Name ++ " ", Error}),
                    handle_error(Config, Module_config, Name, "running fun with name : " ++ Name ++ " ", Error)
    end,
    Config;

handle_msg([Node ,Sensor, Id, Time, {run, Name, Args}], Config, Module_config) when is_list(Name) ->
    lager:info("run fun with name : ~p and arguments : ~p", [Name, Args]),
    Funs = proplists:get_value(funs, Module_config, []),
    Fun = get_fun(Funs, Name),
    try 
        Arguments  = string_to_args("[" ++ Args ++ "]."),
        Result = run_fun(Fun, Name, args_to_types(Arguments)),
        lager:info("Result for fun with name : ~p is : ~p", [Name, Result]),
        create_message_and_send(Module_config, Name,{run_result, Name, {ok,Result}})
    catch 
        _:Error -> create_message_and_send(Module_config, {error, "running fun with name : " ++ Name ++ " and args : ~p " ++ Args ++ " ", Error}),
                    handle_error(Config, Module_config, Name, "running fun with name : " ++ Name ++ " and args : ~p " ++ Args ++ " ", Error)
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

handle_msg([Node ,Driver, Id, Time, Body] = Msg, Config, Module_config) ->
    Funs = proplists:get_value(funs, Module_config, []),
    case get_fun(Funs, {Node, Driver, Id}) of   
       {Name, Fun} -> handle_msg([Node ,Driver, Id, Time, {run, {Node, Driver, Id, Time, Body}}], Config, Module_config);
       [] -> lager:info("no fun found for : ~p", [Msg])
    end;

handle_msg(Msg, Config, Module_config) ->
    lager:warning("~p got an unkown message : ~p", [?MODULE, Msg]),  
    Config.

stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),  
    Config.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
save_fun(Name, Message, Command, Comment, Config, Module_config) ->
    lager:info("save fun under name : ~p with message : ~p ,command : ~p and comment : ~p ", [Name, Message, Command, Comment]),
    Funs = proplists:get_value(funs, Module_config, []),
    try 
        {ok, Fun} = command_to_fun(Command),
        create_message_and_send(Module_config, {save_result, Name, ok}),
        Module_config_1 = lists:keyreplace(funs, 1 , Module_config, {funs, lists:keystore(Name, 1, Funs, {Name, Message, Fun, Command, Comment})}),    
        lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1})
    catch
        _:Error -> create_message_and_send(Module_config, {error, "saving fun with name : " ++ Name ++ " command : " ++ Command, Error}),
                   handle_error(Config, Module_config, Name, "saving fun with name : " ++ Name ++ " command : " ++ Command, Error)
    end.

handle_error(Config, Module_config, Name, Error_text, Error) ->
    Module_config_2 = lists:keyreplace(errors, 1 , Module_config, {errors, [{Name, Error_text, Error}]}),    
    lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_2}).

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

run_fun(Fun, Name, Args) when is_function(Fun) and is_list(Args) ->
    lager:info("1.... : ~p", [Args]),
    Fun(Name, Args);  
run_fun(Fun, Name, Args) when is_function(Fun) ->
    lager:info("2.... : ~p", [Args]),
    Fun(Name, [Args]).      

get_fun(Funs, {Node, Driver, Id} = Msg) ->  
    case lists:keysearch(Msg, 2, Funs) of
        {value, {N, M, F, C, Co}} -> {N,F};
        false -> []
    end;
get_fun(Funs, Name) ->  
    case lists:keysearch(Name, 1, Funs) of
        {value, {N, M, F, C, Co}} -> F;
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
    Message = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "arg_test", [], "fun([X]) -> X + 1 end.", "Das ist ein Argument Test"}), 
    sensor:send_message(nodes(), Message),
    Message1 = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {list}), 
    sensor:send_message(nodes(), Message1),
    Message2 = sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {run, "arg_test", "{1, int}"}), 
    sensor:send_message(nodes(), Message2).

test_me_1() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "send_test", [], "fun([Args]) -> M=sensor:create_message(node(), 'funrunner', {'funrunner test'}), sensor:send_message(nodes(), M)  end.", "Das ist ein Argument Test"}),
    sensor:send_message(nodes(), Message),
    Message1=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {list}), 
    sensor:send_message(nodes(), Message1),
    Message2=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {run, "send_test", []}), 
    sensor:send_message(nodes(), Message2).

test_me_2() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, {<<"horst@ua-TA880GB">>, <<"sample_driver">>, <<"default">>}, "fun([Args]) -> M=sensor:create_message(node(), 'funrunner', {'funrunner test'}), sensor:send_message(nodes(), M), {ok}  end.", "Das ist ein Argument Test"}),
    sensor:send_message(Message).

test_me_3() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {run, {<<"horst@ua-TA880GB">>,<<"sample_driver">>,<<"default">>,<<"63559059448">>,{temp, 10.0}}}), 
    sensor:send_message(Message).

%%{"Licht","11111 2","1"}

test_me_4() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "msg_test", {<<"horst@ua-TA880GB">>, <<"sample_driver">>, <<"default">>}, "fun(Name, [Args]) -> M=sensor:create_message(node(), Name, {\"Licht\",\"11111 2\",\"1\"}), sensor:send_message(M)  end.", "Anschalten Licht"}),
    sensor:send_message(Message).

test_schimmel() ->
    Message=sensor:create_message('node@localhost', 'testmodule', sensor:get_id([]), {save, "schimmel", {<<"horst@raspberrypi">>,<<"dht22_driver">>,<<"default">>}, "fun(Name, [{temp, Temp}, {hum, Hum}]) -> io:format(\"~p~p~n\", [Temp, Hum]) end.", "Schimmel App"}),
    sensor:send_message(Message).

test_schimmel_data() ->
    Message=sensor:create_message('horst@raspberrypi', 'dht22_driver', [{temp, 10.0},{hum, 50}]),
    sensor:send_message(Message).

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