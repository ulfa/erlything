
-define(RECEIVER_OPTIONS,[ 
				binary,
				{reuseaddr, true},				
				{ip, get_env(multi_ip)},
				{multicast_loop,true}, 				
				{broadcast, true}, 
				{active,true},
				{add_membership, {get_env(multi_ip), get_env(ip)}}]).
	
-define(SENDER_OPTIONS,[ 
				binary,
				{multicast_ttl, 32},	
				{multicast_loop,true},		
				{ip, get_env(ip)}]).

-define(ALIVE, <<"Alive">>).
-define(DEAD, <<"Dead">>).
-define(UNKNOWN, <<"Unknown">>).
-define(MESSAGE_ALIVE(Message), {external_interrupt, sue, info, {alive, Message}}).
-define(MESSAGE_DEAD(Message), {external_interrupt, sue, error, {dead, Message}}).


