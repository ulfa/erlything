%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(sue_etop).

-include_lib("runtime_tools/include/observer_backend.hrl").
%% Application callbacks
-export([collect/0, pid_info/1]).


collect() ->
    EtopProcInfo = etop_collect(processes(), []),
    EtopInfo = #etop_info{now = now(),
           n_procs = length(EtopProcInfo),
           run_queue = erlang:statistics(run_queue),
           wall_clock = erlang:statistics(wall_clock),
           runtime = erlang:statistics(runtime),
           memi = etop_memi(),
           procinfo = []
          },
	{record_to_proplist(EtopInfo), recordlist_to_proplist(lists:reverse(EtopProcInfo), [])}.

etop_memi() ->
    try
  [{total, c:memory(total)},
   {processes, c:memory(processes)},
   {ets, c:memory(ets)},
   {atom, c:memory(atom)},
   {code, c:memory(code)},
   {binary, c:memory(binary)}]
    catch
  error:notsup ->
      undefined
    end.

etop_collect([P|Ps], Acc) when P =:= self() ->
    etop_collect(Ps, Acc);
etop_collect([P|Ps], Acc) ->
    Fs = [registered_name,initial_call,memory,reductions,current_function,message_queue_len],
    case process_info(P, Fs) of
      undefined -> etop_collect(Ps, Acc);
  [{registered_name,Reg},{initial_call,Initial},{memory,Mem},
   {reductions,Reds},{current_function,Current},{message_queue_len,Qlen}] ->
      Name = case Reg of
           [] -> Initial;
           _ -> Reg
       end,
      Info = #etop_proc_info{pid=pid_to_list(P) ,mem=Mem, reds=Reds, name=convert_name(Name), cf=cf_to_string(Current),mq=Qlen},
      etop_collect(Ps, [Info|Acc])
    end;
etop_collect([], Acc) -> Acc.

pid_info(Pid) when is_list(Pid) ->
  pid_info(list_to_pid(Pid));

pid_info(Pid) when is_pid(Pid) ->
  case erlang:process_info(Pid) of
    undefined -> [];
    Info -> I1 = convert_info(current_function, Info),
            I2 = convert_info(links, I1),
            convert_info(group_leader, I2)
  end.

convert_info(current_function, List) ->
  Value = proplists:get_value(current_function, List),
  convert_info(current_function, cf_to_string(Value), List);

convert_info(links, List) ->
  Value = proplists:get_value(links, List),
  convert_info(links, pid_link_to_string(Value), List);

convert_info(group_leader, List) ->
  Value = proplists:get_value(group_leader, List),
  convert_info(group_leader, pid_to_list(Value), List).  

convert_info(Key, Value, List) ->
  lists:keyreplace(Key, 1, List, {Key, Value}).

pid_link_to_string(Pids) ->
  [to_list(P)||P<-Pids].

to_list(Pid) when is_pid(Pid) ->
  erlang:pid_to_list(Pid);
to_list(Port) when is_port(Port) ->
  erlang:port_to_list(Port).
  

cf_to_string({M,F,A}) ->
  lists:flatten(io_lib:fwrite("~s:~s/~p", [M, F, A])).

convert_name(Value) when is_atom(Value) ->
  Value;
convert_name({M,F,A}) ->
  "".
recordlist_to_proplist([], Acc) ->
	Acc;
recordlist_to_proplist([H|T], Acc) ->
	recordlist_to_proplist(T, [record_to_proplist(H)|Acc]).

record_to_proplist(#etop_info{} = Rec) ->
  lists:zip(record_info(fields, etop_info), tl(tuple_to_list(Rec)));	
  
record_to_proplist(#etop_proc_info{} = Rec) ->
  lists:zip(record_info(fields, etop_proc_info), tl(tuple_to_list(Rec))).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

record_to_proplist_list_test() ->
	A=[#etop_proc_info{}, #etop_proc_info{}],
	?assertEqual(2,erlang:length(recordlist_to_proplist(A, []))).
	
record_to_proplist_test() ->
	A = #etop_info{},
	?assertEqual([{now,{0,0,0}},
                  {n_procs,0},
                  {wall_clock,{0,0}},
                  {runtime,{0,0}},
                  {run_queue,0},
                  {alloc_areas,[]},
				          {memi, [{total, 0},
				  		    {processes, 0},
						      {ets, 0},
						      {atom, 0},
						      {code, 0},
     					    {binary, 0}]},
				          {procinfo,[]}],
                  record_to_proplist(A)),
  B = #etop_proc_info{pid="<0.1.0>"},  
	?assertEqual([{pid,"<0.1.0>"},
				 {mem,0},
				 {reds,0},
				 {name,undefined},
				 {runtime,0},
				 {cf,undefined},
				 {mq,0}], record_to_proplist(B)).

-endif.