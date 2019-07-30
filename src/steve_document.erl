-module(steve_document).
-export([init/0, get_document/1, set_document/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the documents module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  ets:new(documents, [named_table, set, public]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get a stored document
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_document(DocumentId) ->
   case ets:lookup(documents, DocumentId) of
     [] ->
       notfound;
     [{_, Document}] ->
       Document
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set a document and stream the json-patch changes
% TODO
% - Serialize document updates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_document(DocumentId, NewDocument) ->
  OldDocument = get_document(DocumentId),

  % Insert the document so we can retrieve it later
  ets:insert(documents, {DocumentId, NewDocument}),

  case OldDocument of
    notfound ->
      JsonPayload = jsone:encode(NewDocument),
      steve_channel:send_event(DocumentId, "message", JsonPayload);
    _ ->
      % Diff the two documents and build a JSON payload
      Diffs = steve_util:map_diff(OldDocument, NewDocument),
      Payload = create_payload(Diffs),

      case Payload of
        nochanges ->
          ok;
        _ ->
          JsonPayload = jsone:encode(Payload),
          % Stream the JSON payload to connected clients
          steve_channel:send_event(DocumentId, "message", JsonPayload)
      end
  end.

create_payload(Diffs) ->
  create_payload(Diffs, []).

create_payload({}, []) ->
  nochanges;

create_payload([], []) ->
  nochanges;

create_payload([H|T], Payloads) ->
  case H of
    {add, Path, NewValue} ->
      create_payload(T, Payloads ++ [#{<<"op">> => <<"add">>,
                                   <<"path">> => format_path(Path),
                                   <<"value">> => NewValue}]);
    {remove, Path} ->
      create_payload(T, Payloads ++ [#{<<"op">> => <<"remove">>,
                                   <<"path">> => format_path(Path)}]);
    {replace, Path, NewValue} ->
      create_payload(T, Payloads ++ [#{<<"op">> => <<"replace">>,
                                   <<"path">> => format_path(Path),
                                   <<"value">> => NewValue}]);
     _ ->
       create_payload(T, Payloads)
  end;

create_payload([], NewPayloads) ->
  NewPayloads.

format_path(Path) ->
  list_to_binary("/" ++ lists:join("/", Path)).
