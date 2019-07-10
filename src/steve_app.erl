-module(steve_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    % Create a process group that we will have the SSE
    % streaming handler report to
    pg2:create(stream_processes),

    QueuePid = steve_queue:new(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/stream", steve_stream_handler, []},
            {"/broadcast", steve_broadcast_handler, [QueuePid]}
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
