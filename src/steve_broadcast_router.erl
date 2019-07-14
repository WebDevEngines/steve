-module(steve_broadcast_router).

-export([start/0]).

start() ->
  ets:new(streams, [named_table, set, public]),
  spawn(fun() -> broadcast_loop(0) end).

broadcast_loop(N) ->
  receive
    {register, StreamId, StreamPid} ->
      ets:insert(streams, {StreamPid, StreamId}),
      broadcast_loop(N+1); 
    {unregister, StreamPid} ->
      ets:delete(streams, StreamPid),
      broadcast_loop(N-1);
    {broadcast, StreamId, Msg} ->
      StreamPids = ets:match_object(streams, {'_', StreamId}),
      send_messages(StreamPids, Msg),
      broadcast_loop(N);
    {cleanup_connections} ->
      StreamPids = e
    {num_connections, Pid} ->
      Pid ! {num_connections, N},
      broadcast_loop(N)
  end.

send_messages([H|T], Msg) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, ["msg: ", Msg, "\n\n"]};
    false ->
      self() ! {unregister, StreamPid}
  end,
  send_messages(T, Msg);

send_messages([], _) ->
  ok.