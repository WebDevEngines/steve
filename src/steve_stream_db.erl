-module(steve_stream_db).

-export([init/0, get_pids/1, add_pid/2, remove_pid/1, num_pids/0]).

init() ->
  ets:new(streams, [named_table, set, public]),
  ok.

get_pids(Channel) ->
  ets:match_object(streams, {'_', Channel}).

add_pid(Pid, Channel) ->
  ets:insert(streams, {Pid, Channel}).

remove_pid(Pid) ->
  ets:delete(streams, Pid).

num_pids() ->
  ets:info(streams, size).
