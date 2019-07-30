-module(steve_stream_handler).
-export([init/2, info/3, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the handler. Called on each HTTP request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  #{documentId := DocumentId} = cowboy_req:match_qs([documentId], Req),

  % Check authorization status
  %
  %

  % Register this stream handler instance with the channel
  steve_channel:add_stream(self(), DocumentId),

  % Prep response
  Resp = cowboy_req:stream_reply(
    200, #{<<"content-type">> => <<"text/event-stream">>}, Req),

  % Retrieve the document by id
  Document = steve_document:get_document(DocumentId),

  case Document of
    notfound ->
      ok;
    _ ->
      % Stream initial document
      Payload = steve_channel:create_payload(jsone:encode(Document)),
      cowboy_req:stream_body(Payload, nofin, Resp)
  end,

  % Keep connection open so we can keep streaming
  {cowboy_loop, Resp, State}.

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
