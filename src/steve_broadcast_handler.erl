-module(steve_broadcast_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [BroadcastRouter] = State,
  Resp = handle(Req, BroadcastRouter),
  {ok, Resp, State}.

% Broadcast the passed message to all the streams
handle(Req, BroadcastRouter) ->

  % Get the message from the query string
  #{stream_id := StreamId, msg := Msg} = cowboy_req:match_qs([stream_id, msg], Req),

  % Broadcast message to the proper connected streams
  BroadcastRouter ! {broadcast, StreamId, Msg},
  
  % Close the HTTP connection
  cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"Message Sent!">>,
    Req
  ).

terminate(_Reason, _Req, _State) ->
    ok.