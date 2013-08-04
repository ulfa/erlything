-define(SUPERVISOR_CONFIG, "sensor.config").
-define(MESSAGES_CONFIG, "messages.config").
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(THINGS_CONFIG, "things.config").
-define(THING(Name, Config), {Name, {thing, start_link, [Config]}, permanent, 5000, worker, [thing]}).