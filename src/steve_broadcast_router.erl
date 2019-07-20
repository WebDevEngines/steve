-module(steve_broadcast_router).

-export([start/0]).

start() ->
  spawn(fun() -> broadcast_loop() end).

broadcast_loop() ->
  receive
    {broadcast, Event, Data} ->
      EventStreamPids = ets:match_object(event_streams, {'_', Event}),
      send_events(EventStreamPids, Data),
      broadcast_loop()
  end.

send_events([H|T], Data) ->
  {EventStreamPid, _} = H,
  case is_process_alive(EventStreamPid) of
    true ->
      EventStreamPid ! {event, ["data: ", Data, "\n\n"]};
    false ->
      ets:delete(event_streams, EventStreamPid)
  end,
  send_events(T, Data);

send_events([], _) ->
  ok.