-module(steve_stream_handler).
-export([init/2, info/3, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the handler. Called on each HTTP request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  #{channel := Channel} = cowboy_req:match_qs([channel], Req),

  % Register this stream handler instance with the channel
  steve_channel:add_stream(self(), Channel),

  % Retrieve payloads newer that tne passed last-event-id header if passed
  LastEventId = cowboy_req:header(<<"last-event-id">>, Req),
  LastEventPayloads = get_last_event_payloads(Channel, LastEventId),

  % Prep response
  Resp = cowboy_req:stream_reply(
    200, #{<<"content-type">> => <<"text/event-stream">>}, Req),

  % Stream old payloads if necessary
  ok = stream_last_event_payloads(Resp, LastEventPayloads),

  % Keep connection open so we can keep streaming
  {cowboy_loop, Resp, State}.

get_last_event_payloads(_, undefined) ->
  [];

get_last_event_payloads(Channel, LastEventId) ->
  steve_channel:get_payloads_after(Channel, LastEventId).

stream_last_event_payloads(_, []) ->
  ok;

stream_last_event_payloads(Req, [H|T]) ->
  cowboy_req:stream_body(H, nofin, Req),
  stream_last_event_payloads(Req, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If the handler receives an unexpected EOF from the client then stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(eof, Req, State) ->
  {stop, Req, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If the handler received an 'event' message then stream the payload
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info({event, Data}, Req, State) ->
  cowboy_req:stream_body(Data, nofin, Req),
  {ok, Req, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If any other messages are encountered don't do anything
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(_Msg, Req, State) ->
  {ok, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% When the handler terminated clean up
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _Req, _State) ->
  ok.
