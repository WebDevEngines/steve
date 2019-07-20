-module(steve_broadcast_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [BroadcastRouter] = State,
  Resp = handle(Req, BroadcastRouter),
  {ok, Resp, State}.

% Broadcast the passed message to all the streams
handle(Req, BroadcastRouter) ->

  % Get the event and data
  #{event := Event, data := Data} = cowboy_req:match_qs([event, data], Req),

  % Broadcast data to the proper events
  BroadcastRouter ! {broadcast, Event, Data},
  
  % Close the HTTP connection
  cowboy_req:reply(
    202,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"">>,
    Req
  ).

terminate(_Reason, _Req, _State) ->
    ok.