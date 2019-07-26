-module(steve_broadcast_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [ChannelRouter] = State,
  Resp = handle(Req, ChannelRouter),
  {ok, Resp, State}.

handle(Req, ChannelRouter) ->
  #{channel := Channel, 
    event   := Event, 
    data    := Data} = cowboy_req:match_qs([channel, event, data], Req),

  ChannelRouter ! {broadcast, Channel, Event, Data},
  
  cowboy_req:reply(
    202,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"">>,
    Req
  ).

terminate(_Reason, _Req, _State) ->
    ok.