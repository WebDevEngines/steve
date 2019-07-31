-module(steve_document_change_handler).
-export([init/2, info/3, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the handler. Called on each HTTP request
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  AuthToken = cowboy_req:parse_header(<<"authorization">>, Req),
  DocumentId = cowboy_req:binding(documentId, Req),

  % Check authorization status
  case steve_auth:is_authorized(AuthToken, DocumentId) of
    false ->
      Resp = cowboy_req:reply(401,
        #{<<"content-type">> => <<"text/event-stream">>},
        <<"Unauthorized">>, Req),
      {ok, Resp, State};
    true ->
      % Register this stream handler instance with the channel
      steve_channel:add_stream(self(), DocumentId),

      % Prep response
      Resp = cowboy_req:stream_reply(
        200, #{<<"content-type">> => <<"text/event-stream">>}, Req),

      % Retrieve the document by id
      Document = steve_document:get_document(DocumentId),

      case Document of
        notfound ->
          % Wait for a document with a matching documentId to be added
          ok;
        _ ->
          % Stream initial document
          Payload = steve_channel:create_payload(jsone:encode(Document)),
          cowboy_req:stream_body(Payload, nofin, Resp)
      end,

      % Keep connection open so we can keep streaming
      {cowboy_loop, Resp, State}
  end.

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
% When the handler terminates clean up
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _Req, _State) ->
  ok.
