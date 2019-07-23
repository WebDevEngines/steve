-module(steve_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  ets:new(streams, [named_table, set, public]),

  BroadcastRouter = steve_broadcast_router:start(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/streams", steve_stream_handler, [BroadcastRouter]},
      {"/stats", steve_stats_handler, [BroadcastRouter]}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(
    http,
    [
      {port, 8093},
      {max_connections, infinity}
    ],
    #{env => #{dispatch => Dispatch}, idle_timeout => 600000}
  ),
  steve_sup:start_link().

stop(_State) ->
  ok.
