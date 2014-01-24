
-define(RECEIVER_OPTIONS,[ 
				binary,
				{reuseaddr, true},				
				{ip, get_env(multi_ip)},
				{multicast_loop,true}, 				
				{broadcast, true}, 
				{active,true},
				{add_membership, {get_env(multi_ip), ip_device:get_ip()}}]).
	
-define(SENDER_OPTIONS,[ 
				binary,
				{multicast_ttl, 32},	
				{multicast_loop,true},		
				{ip, ip_device:get_ip()}]).

-define(ALIVE, <<"Alive">>).
-define(DEAD, <<"Dead">>).
-define(UNKNOWN, <<"Unknown">>).
-define(MESSAGE(Body), {external_interrupt, cuberl, Body}).
-define(MESSAGE_ERROR(Message), {external_interrupt, cuberl, {error, Message}}).
-define(MESSAGE_ERROR(Message, Reason), {external_interrupt, cuberl, {error, Message, Reason}}).
-define(MESSAGE_INFO(Message), {external_interrupt, cuberl, {info, Message}}).

