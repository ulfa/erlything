-define(SUPERVISOR_CONFIG, "sensor.config").
-define(MESSAGES_CONFIG, "messages.config").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).