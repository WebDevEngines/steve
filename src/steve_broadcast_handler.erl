-module(steve_broadcast_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [QueuePid] = State,
	Resp = handle(Req, QueuePid),
	{ok, Resp, State}.

% Broadcast the passed message to all the open streams
handle(Req, QueuePid) ->
  % Get the message from the query string
  #{msg := Msg} = cowboy_req:match_qs([msg], Req),

  % Send a message to the queue process which will send 
  % to the connected streams
  QueuePid ! {send, Msg},
  
  % Close the HTTP connection
  cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"Message Sent!">>,
    Req
  ).

terminate(_Reason, _Req, _State) ->
    ok.
