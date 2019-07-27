-module(steve_broadcast_router).

-export([start/0]).

start() ->
  spawn(fun() -> broadcast_router_loop() end).

% Send payload to the streams connected to the passed channel
broadcast_router_loop() ->
  receive
    {broadcast, Channel, Event, Data} ->
      StreamPids = steve_stream:get_pids(Channel),
      Payload = steve_broadcast:create_event_payload(Channel, Event, Data),
      ok = send_event(StreamPids, Payload),
      broadcast_router_loop()
  end.

send_event([H|T], Payload) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, Payload};
    false ->
      steve_stream:remove_pid(StreamPid)
  end,
  send_event(T, Payload);

send_event([], _) ->
  ok.

