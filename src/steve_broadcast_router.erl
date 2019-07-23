-module(steve_broadcast_router).

-export([start/0]).

start() ->
  spawn(fun() -> 
    quickrand:seed(),
    broadcast_loop() 
  end).

broadcast_loop() ->
  receive
    {broadcast, Channel, Event, Data} ->
      EventStreamPids = ets:match_object(streams, {'_', Channel}),
      send_events(EventStreamPids, Event, Data),
      broadcast_loop()
  end.

send_events([H|T], Event, Data) ->
  {EventStreamPid, _} = H,
  case is_process_alive(EventStreamPid) of
    true ->
      Id = uuid:uuid_to_string(uuid:get_v4_urandom()),
      EventStreamPid ! {event, [
        "event: ", Event, "\n", 
        "id: ", Id, "\n", 
        "data: ", Data, "\n\n"
      ]};
    false ->
      ets:delete(streams, EventStreamPid)
  end,
  send_events(T, Event, Data);

send_events([], _, _) ->
  ok.