-module(steve_stream_handler).

-export([init/2, info/3, terminate/3]).

init(Req, State) ->
  #{channel := Channel} = cowboy_req:match_qs([channel], Req),
  LastEventId = cowboy_req:header(<<"last-event-id">>, Req),
  LastEventPayloads = get_last_event_payloads(Channel, LastEventId),
  steve_stream_db:add_pid(self(), Channel),
  Resp = cowboy_req:stream_reply(200, Req),
  stream_last_event_payloads(Resp, LastEventPayloads),
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

get_last_event_payloads(_, undefined) ->
  ok;

get_last_event_payloads(Channel, LastEventId) ->
  steve_channel_router:get_payloads_after(Channel, LastEventId).

stream_last_event_payloads(_, ok) ->
  ok;

stream_last_event_payloads(_, []) ->
  ok;

stream_last_event_payloads(Req, [H|T]) ->
  cowboy_req:stream_body(H, nofin, Req),
  stream_last_event_payloads(Req, T).