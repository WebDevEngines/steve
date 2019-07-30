-module(steve_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(API_PORT, os:getenv("API_PORT", 8093)).
-define(API_IDLE_TIMEOUT_MS, os:getenv("API_IDLE_TIMEOUT_MS", 600000)).

start(_Type, _Args) ->
  ok = steve_channel:init(),
  ok = steve_document:init(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/stream", steve_stream_handler, []},
      {"/document", steve_document_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(
    http,
    [
      {port, ?API_PORT},
      {max_connections, infinity}
    ],
    #{env => #{dispatch => Dispatch}, idle_timeout => ?API_IDLE_TIMEOUT_MS}
  ),
  steve_sup:start_link().

stop(_State) ->
  ok.
