
-define(MESSAGES_CONFIG, "messages.config").
-define(THINGS_CONFIG, "things.config").

-define(APPLICATION, horst).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(THING(Name, Config), {Name, {thing, start_link, [Config]}, transient, 5000, worker, [thing]}).
-define(SEND(Body), sensor:send(Config, ?MODULE, Body)).
-define(SEND(Module, Body), sensor:send([], Module, Body)).
-define(SEND_1(Module, Body), sensor:send(Config, Module, Body)).

-define(TABLE, table_id).

-define(SYSTEM, 'system').
-define(ON, "on").
-define(OFF, "off").
-define(RISING, "on").
-define(FALLING, "off").
-define(THING_CONFIG, "things.config").
-define(STARTED, "started").
-define(STOPPED, "stopped").

-define(MAX_QUEUE_LENGTH, 30).
-define(MQTT_PREFIX, "erlyThing").



