-module(steve_num_connections_handler).

-export([init/2, handle/2, terminate/3]).

init(Req, State) ->
  [BroadcastRouter] = State,
  Resp = handle(Req, BroadcastRouter),
  {ok, Resp, State}.

handle(Req, BroadcastRouter) ->
  BroadcastRouter ! {num_connections, self()},
  receive
    {num_connections, NumConnections} ->
      cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
        list_to_binary(integer_to_list(NumConnections)),
        Req
      )
  end.

terminate(_Reason, _Req, _State) ->
    ok.