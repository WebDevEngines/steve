-module(steve_stream_handler).

-export([init/2, info/3, terminate/3]).

init(Req, State) ->
  [BroadcastRouter] = State,
  #{stream_id := StreamId} = cowboy_req:match_qs([stream_id], Req),
  BroadcastRouter ! {register, StreamId, self()},
  Resp = cowboy_req:stream_reply(200, Req),
  {cowboy_loop, Resp, State}.

info(eof, Req, State) ->
  {stop, Req, State};

info({event, Data}, Req, State) ->
  cowboy_req:stream_body(Data, nofin, Req),
  {ok, Req, State};

info(_Msg, Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.
