
-define(MESSAGES_CONFIG, "messages.config").
-define(THINGS_CONFIG, "things.config").

-define(APPLICATION, horst).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(THING(Name, Config), {Name, {thing, start_link, [Config]}, transient, 5000, worker, [thing]}).