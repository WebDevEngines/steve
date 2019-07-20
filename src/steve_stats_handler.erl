-module(steve_stats_handler).

-export([init/2, handle/1, terminate/3]).

init(Req, State) ->
  Resp = handle(Req),
  {ok, Resp, State}.

handle(Req) ->
  NumConnections = ets:info(event_streams, size),
  cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    list_to_binary(integer_to_list(NumConnections)),
    Req
  ).

terminate(_Reason, _Req, _State) ->
    ok.