-module(dsl).

-include("../include/horst.hrl").

-export([create_rules/2, apply_rules/4]).


create_rules(Module, Rules) ->
    [rule_to_function(Module, Rule) || Rule <- Rules].

rule_to_function(Module, Rule) ->
    {ok, Scanned, _} = erl_scan:string(Rule),
    [{'when',_}, {_,_,Variable}, {_,_,Operator}, {Limit_type, _, Limit},
     {_,_,'than'}, {_,_, message}, {'{', 1}, {_,_, Message_type},  {',',1}, {Action_type, _,Action_value}, {'}', 1}] = Scanned,
    to_function(Module, Variable, Operator, Limit_type, Limit, Message_type, Action_type, Action_value).

to_function(Module, Variable, Operator, Limit_type, Limit, Message_type, Action_type, Action_value) ->
    fun(Config, Node, {Variable, Value}) ->
        if
            (Value < Limit andalso Operator =:= less) ->                
                ?SEND([{Message_type, Node, Action_value}]);                
            (Value > Limit andalso Operator =:= greater) ->                
                ?SEND([{Message_type, Node, Action_value}]);
        true ->
            lager:debug("no rule applied")
        end
    end.

apply_rules(Config, Funs, Node, {temp, _Temp} = Data) ->
    [Fun(Config, Node, Data) || Fun <- Funs].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
create_rules_test() ->
    A=["when temp greater 30.0 than message {warning, \"temp is too high\"}",
        "when temp less 30.0 than message {info, \"temp is normal again\"}"],
        create_rules(os_display_driver, A).
rule_to_function_test() ->
    Rule = "when temp greater 30.0 than message {warning, \"temp is too high\"}",
    rule_to_function(os_display_driver, Rule).
to_function_test() ->
    A = to_function(os_display_driver, temp, greater, integer, 30.0, warning, string, "temp is too high"),
    A(node(), {temp, 40.0}),
    B = to_function(os_display_driver, temp, less, integer, 30.0, info, string, "temp is normal again"),
    B(node(),{temp, 29.0}).
parse_test() ->
    Rule = "when temp greater 30.0 than message {warning, \"temp is too high\"}",
    {ok, Scanned, _} = erl_scan:string(Rule),
    [erl_scan:token_info(Token) || Token <- Scanned ].

-endif.