-module(steve_broadcast_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [BroadcastRouter] = State,
  Resp = handle(Req, BroadcastRouter),
  {ok, Resp, State}.

% Handle broadcast requests by sending a message to the broadcast router so
% it can figure out which streams to send the payload it creates to
handle(Req, BroadcastRouter) ->
  #{channel := Channel, event := Event, data := Data} = cowboy_req:match_qs([channel, event, data], Req),
  BroadcastRouter ! {broadcast, Channel, Event, Data},
  cowboy_req:reply(202, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, <<"">>, Req).

terminate(_Reason, _Req, _State) ->
    ok.