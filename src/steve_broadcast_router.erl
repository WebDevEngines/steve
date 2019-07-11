-module(steve_broadcast_router).

-export([new/0]).

new() ->
  M = maps:new(),
  spawn(fun() -> loop(M) end).

loop(M) ->
  receive
    {register, StreamId, StreamPid} ->
      StreamPids = maps:get(StreamId, M, []),
      loop(maps:put(StreamId, StreamPids ++ [StreamPid], M)); 
    {broadcast, StreamId, Msg} ->
      StreamPids = maps:get(StreamId, M, []),
      send_messages(StreamPids, Msg),
      loop(M)
  end.

send_messages([H|T], Msg) ->
  H ! {event, ["msg: ", Msg, "\n\n"]},
  send_messages(T, Msg);

send_messages([], _) ->
  ok.