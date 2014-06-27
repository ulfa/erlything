%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(funrunner_driver).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/horst.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, stop/1]).
-export([handle_msg/3]).
-export([test_me/0 ,test_exception/0]).
-export([test_schimmel1/0, test_schimmel_data/0, test_send_after/0, test_start_send_after/0]).

init(Config) ->
    lager:info("~p:init('~p')", [?MODULE, Config]),
    {ok, Config}.

stop(Config) ->
    lager:info("~p:stop('~p')", [?MODULE, Config]),
    {ok, Config}.

handle_msg([Node,_Sensor, _Id, Time, {run_result, Name, {ok, Result}}], Config, _Module_config) ->  
    lager:info("get result for fun : ~p with result : ~p", [Name, Result]),  
    Table_Id = proplists:get_value(?TABLE, Config),
    [{results, Data}] = ets:lookup(Table_Id, results),
    ets:insert(Table_Id, [{results, add(Data, {Time, Node, Name, Result})}]),
    Config;

handle_msg([Node, _Sensor, _Id, Time, {error, Text, Reason}], Config, _Module_config) ->
    Table_Id = proplists:get_value(?TABLE, Config),
    [{errors, Data}] = ets:lookup(Table_Id, errors),
    ets:insert(Table_Id, [{errors, add(Data, {Time, Node, Text, Reason})}]),
    Config;


handle_msg([_Node,_Sensor, _Id, _Time, {save, Name, Message, Command, Comment}], Config, Module_config) ->
    save_fun({save, Name, Message, Command, Comment}, Config, Module_config);

handle_msg([_Node, _Sensor, _Id, _Time, {run, {Node_1, Driver_1, Id_1, _Time_1, Body} = Msg}], Config, Module_config) ->
    Msg = {Node_1, Driver_1, Id_1, Body},
    lager:info("run fun for message : ~p ", [Msg]),
    Funs = config:get_value(funs, Module_config, []),
    {Name, Fun} = get_fun(Funs, {Node_1, Driver_1, Id_1}),      
    try
        Result = run_fun(Fun, Name, Body),
        lager:info("Result of fun : ~p is : ~p", [Msg, Result]),
        create_message_and_send(Config, {run_result, Name, {ok, Result}})
    catch   
        _:Error -> create_message_and_send(Config, {error, "running fun with name : " ++ Name ++ " ", Error}),
                    handle_error(Config, Module_config, Name, "running fun with name : " ++ Name ++ " ", Error)
    end,
    Config;

handle_msg([_Node, _Sensor, _Id, _Time, {run, Name, Args}], Config, Module_config) when is_list(Name) ->
    lager:info("run fun with name : ~p and arguments : ~p", [Name, Args]),
    Funs = config:get_value(funs, Module_config, []),
    case get_fun(Funs, Name) of
        [] -> Config;
        Fun ->
            try 
                Arguments  = string_to_args("[" ++ Args ++ "]."),        
                Result = run_fun(Fun, Name, args_to_types(Arguments)),
                lager:info("Result for fun with name : ~p is : ~p", [Name, Result]),
                create_message_and_send(Config, Name, {run_result, Name, {ok, Result}})
            catch 
                _:Error -> lager:error("fun : ~p Error : ~p", [Name, Error]),
                           create_message_and_send(Config, {error, "running fun with name : " ++ Name ++ " and args :" ++ Args ++ " ", Error}),
                           handle_error(Config, Module_config, Name, "running fun with name : " ++ Name ++ " and args : " ++ Args ++ " ", Error)
            end,
            Config
    end;

handle_msg([Node, Driver, Id, Time, {run, Args}], Config, Module_config) ->
    lager:info("..... run fun with and arguments : "),
    Funs = config:get_value(funs, Module_config, []),
    case get_fun(Funs, {Node, Driver, Id}) of
        [] -> lager:info("no fun for Message ~p found", [{Node, Driver, Id}]);
        {Name, Fun} ->  try
                            Result = run_fun(Fun, Name, Args),
                            lager:info("Result of fun : ~p is : ~p", [Name, Result]),
                            create_message_and_send(Config, {run_result, Name, {ok, Result}})
                        catch   
                            _:Error -> create_message_and_send(Config, {error, "running fun with name : " ++ Name ++ " ", Error}),
                            handle_error(Config, Module_config, Name, "running fun with name : " ++ Name ++ " ", Error)
                        end
    end,
    Config;

handle_msg([_Node, _Sensor, _Id, _Time, {list, Name}], Config, Module_config) ->
    lager:info("list the fun with name : ~p", [Name]),
    Funs = config:get_value(funs, Module_config, []),
    Result = get_command(Funs, Name),
    create_message_and_send(Config, {list_result, Name, {ok, Result}}),
    Config;

handle_msg([_Node, _Sensor, _Id, _Time, {list}], Config, Module_config) ->
    lager:info("list all funs "),
    Funs = config:get_value(funs, Module_config, []),
    Result = get_commands(Funs),
    create_message_and_send(Config, {list_result, {ok, Result}}),
    Config;

handle_msg([_Node, _Sensor, _Id, _Time, {save_result, _Msg}], Config, _Module_config) ->
    Config;

handle_msg([Node ,Driver, Id, Time, Body] = Msg, Config, Module_config) ->
    Funs = config:get_value(funs, Module_config, []),
    case get_fun(Funs, {Node, Driver, Id}) of   
       {_Name, _Fun} -> handle_msg([Node ,Driver, Id, Time, {run, {Node, Driver, Id, Time, Body}}], Config, Module_config);
       [] -> lager:info("no fun found for : ~p", [Msg]),
             Config
    end;

handle_msg(Msg, Config, _Module_config) ->
    lager:warning("~p got an unkown message : ~p", [?MODULE, Msg]),  
    Config.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
save_fun({save, Name, Message, Command, Comment} = Msg , Config, Module_config) ->
    lager:info("save fun under name : ~p with message : ~p ,command : ~p and comment : ~p ", [Name, Message, Command, Comment]),
    Funs = config:get_value(funs, Module_config, []),
    try 
        {ok, Fun} = command_to_fun(Command),
        create_message_and_send(Module_config, {save_result, Msg}),
        Module_config_1 = lists:keyreplace(funs, 1 , Module_config, {funs, lists:keystore(Name, 1, Funs, {Name, Message, Fun, Command, Comment})}),    
        lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_1})        
    catch
        _:Error -> create_message_and_send(Module_config, {error, "saving fun with name : " ++ Name ++ " command : " ++ Command, Error}),
                   handle_error(Config, Module_config, Name, "saving fun with name : " ++ Name ++ " command : " ++ Command, Error)
    end.

handle_error(Config, Module_config, Name, Error_text, Error) ->
    Module_config_2 = lists:keyreplace(errors, 1 , Module_config, {errors, [{Name, Error_text, Error}]}),    
    lists:keyreplace(driver, 1, Config, {driver, {?MODULE, handle_msg}, Module_config_2}).

create_message_and_send(Config, Result) ->
    create_message_and_send(Config, config_handler:get_id(Config), Result).

create_message_and_send(Config, _Id, Result) ->    
    ?SEND(Result).


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

convert(Value, atm) when is_atom(Value) ->
    Value;

convert(Value, str) when is_list(Value) ->
    Value.

run_fun(Fun, Name, Args) when is_function(Fun) and is_list(Args) ->
    lager:info("~p ~p ~w" , [Fun, Name, Args]),
    Fun(self(), Name, Args);  
run_fun(Fun, Name, Args) when is_function(Fun) ->
    lager:info("~p ~p ~w" , [Fun, Name, Args]),
    Fun(self(),Name, [Args]).      

get_fun(Funs, {Node, Driver, Id} = Msg) ->  
    case lists:keysearch(Msg, 2, Funs) of
        {value, {N, _M, F, _C, _Co}} -> {N,F};
        false -> []
    end;
get_fun(Funs, Name) ->  
    case lists:keysearch(Name, 1, Funs) of
        {value, {_N, _M, F, _C, _Co}} -> F;
        false -> []
    end.


get_commands(Funs) ->
    [{N, C, Co} || {N, _F, C, Co} <- Funs].  
get_command(Funs, Name) ->
    case lists:keysearch(Name, 1, Funs) of
        {value, {N, _F, C, Co}} -> {N, C, Co};
        false -> []
    end.    

add([], Value) ->
    [Value];
add(List, Value) ->
    case length(List) =< ?MAX_QUEUE_LENGTH of
        true -> [Value|List];
        false -> [Value|lists:sublist(List, ?MAX_QUEUE_LENGTH)]
    end.    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
test_me() ->
    Message = sensor:create_message('node@localhost', 'testmodule', config_handler:get_id([]), {save, "arg_test", [], "fun(Pid, Name,[X]) -> X + 1 end.", "Das ist ein Argument Test"}), 
    sensor:send_message([node()], Message),
    Message1 = sensor:create_message('node@localhost', 'testmodule', config_handler:get_id([]), {list}), 
    sensor:send_message([node()], Message1),
    Message2 = sensor:create_message('node@localhost', 'testmodule', config_handler:get_id([]), {run, "arg_test", "{1, int}"}), 
    sensor:send_message([node()], Message2).

test_schimmel1() ->
    Config = [],
    ?SEND({save, "schimmel", {<<"horst@raspberrypi">>,<<"dht22_driver">>,<<"default">>}, "fun(Pid, Name, [{temp, Temp}, {hum, Hum}]) -> io:format(\"~p~p~n\", [Temp, Hum]) end.", "Schimmel App"}).

test_schimmel_data() ->
    Config = [],
    ?SEND({run, [{temp, 10.0},{hum, 50}]}).

test_send_after() ->
    Config = [],
    Message={save, "send_after", {<<"horst@macbook-pro">>,<<"testmodule">>,<<"default">>}, 
        "fun(Pid, Name, Body) ->                      
            sensor:send_after(Pid, 20000,[{Name, 'Das ist ein send_after test'}]),
            ok 
         end.", "This is a send_after example"},
    ?SEND(Message).

test_start_send_after() ->
    Config = [],
    ?SEND("start send_after test").    

test_exception() ->
    try  
        command_to_fun("fun(X) -> X + 1 end")
    catch
        _:Error -> lager:info("Error : ~p", [Error])
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
string_to_args_test() ->
    ?assertEqual([{1,i}, {2,s}],string_to_args("[{1,i}, {2,s}].")),
    ?assertEqual([{10, int}],string_to_args("[{10,int}].")),
    ?assertEqual([{'Bell', atm}],string_to_args("[{'Bell',atm}].")).

args_to_types_test() ->
    ?assertEqual([1],args_to_types([{1,int}])),
    ?assertEqual(['Bell'], args_to_types([{'Bell',atm}])).

get_fun_test() ->
    Funs = [{"schimmel",{<<"horst@raspberrypi">>,<<"dht22_driver">>,<<"default">>}, "Fun_1" ,"Body_2", "Comment_1"},
            {"schimmel_1",{<<"horst@raspberrypi">>,<<"test_driver">>,<<"default">>}, "Fun_2","Body_1", "Comment_1"}
            ],
    ?assertEqual({"schimmel_1","Fun_2"}, get_fun(Funs, {<<"horst@raspberrypi">>,<<"test_driver">>,<<"default">>})).

fun_test() ->
    fun(Name, Pid, [Body]) -> 
        case Body of 
            "off" -> ok; 
            "on" -> sensor:send([], Name, Body),
                    sensor:send_after(Pid, 20000,[{Name,  "off"}])
        end
    end.
-endif.