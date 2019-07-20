-module(steve_broadcast_router).

-export([start/0]).

start() ->
  spawn(fun() -> broadcast_loop() end).

broadcast_loop() ->
  receive
    {broadcast, StreamId, Msg} ->
      StreamPids = ets:match_object(streams, {'_', StreamId}),
      send_messages(StreamPids, Msg),
      broadcast_loop()
  end.

send_messages([H|T], Msg) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, ["msg: ", Msg, "\n\n"]};
    false ->
      ets:delete(streams, StreamPid)
  end,
  send_messages(T, Msg);

send_messages([], _) ->
  ok.
