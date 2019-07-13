-module(steve_broadcast_router).

-export([start/0]).

start() ->
  M = maps:new(),
  N = 0,
  spawn(fun() -> loop(M, N) end).

loop(M, N) ->
  receive
    {register, StreamId, StreamPid} ->
      StreamPids = maps:get(StreamId, M, []),
      loop(maps:put(StreamId, StreamPids ++ [StreamPid], M), N +1); 
    {broadcast, StreamId, Msg} ->
      StreamPids = maps:get(StreamId, M, []),
      send_messages(StreamPids, Msg),
      loop(M, N);
    {num_connections, Pid} ->
      Pid ! {num_connections, N},
      loop(M, N)
  end.

send_messages([H|T], Msg) ->
  H ! {event, ["msg: ", Msg, "\n\n"]},
  send_messages(T, Msg);

send_messages([], _) ->
  ok.