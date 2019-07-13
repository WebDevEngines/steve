-module(steve_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  BroadcastRouter = steve_broadcast_router:start(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/stream", steve_stream_handler, [BroadcastRouter]},
      {"/broadcast", steve_broadcast_handler, [BroadcastRouter]},
      {"/num_connections", steve_num_connections_handler, [BroadcastRouter]}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(
    http,
    [
      {port, 8093}
    ],
    #{env => #{dispatch => Dispatch}, idle_timeout => 600000}
  ),
  steve_sup:start_link().

stop(_State) ->
  ok.