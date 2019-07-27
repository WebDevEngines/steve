-module(steve_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  ok = steve_stream:init(),
  ok = steve_broadcast:init(),

  BroadcastRouter = steve_broadcast_router:start(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/streams", steve_stream_handler, [BroadcastRouter]},
      {"/broadcast", steve_broadcast_handler, [BroadcastRouter]}
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