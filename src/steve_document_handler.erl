-module(steve_document_handler).
-export([init/2, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the document handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  AuthToken = cowboy_req:parse_header(<<"authorization">>, Req),
  DocumentId = cowboy_req:binding(documentId, Req),
  {ok, DocumentBody, _} = cowboy_req:read_body(Req),
  Headers = #{<<"content-type">> => <<"text/plain; charset=utf-8">>},

  % Check authorization status
  case steve_auth:is_authorized(AuthToken, DocumentId) of
    false ->
      Resp = cowboy_req:reply(401, Headers, <<"Unauthorized">>, Req),
      {ok, Resp, State};
    true ->

      % Check JSON decoding
      DecodingResult = jsone:try_decode(DocumentBody),
      case DecodingResult of
        {error, _} ->
          Resp = cowboy_req:reply(400, Headers, <<"Bad request">>, Req),
          {ok, Resp, State};
        {ok, Document, _} ->

          % Check supported JSON
          case is_map(Document) of
            false ->
              Resp = cowboy_req:reply(400, Headers, <<"Bad request">>, Req),
              {ok, Resp, State};
            true ->
              steve_document:set_document(DocumentId, Document),
              Resp = cowboy_req:reply(202, Headers, <<"Accepted">>, Req),
              {ok, Resp, State}
          end
      end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cleanup after the handler exits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _Req, _State) ->
    ok.
