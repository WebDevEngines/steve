-module(steve_broadcast).
-export([init/0, create_event_payload/3, get_payloads_after/2]).
-define(HASH_MIN_LENGTH, 16).
-define(MAX_PAYLOADS_STORED_PER_CHANNEL, 1000).

init() ->
  ets:new(channels, [named_table, set, public]),
  ok.

% Create an event payload from the passed ata and store it
% for later retrieval via last-event-id
create_event_payload(Channel, Event, Data) ->
  Result = get_payloads(Channel),
  {HashCtx, EventPayloads} = get_hash_ctx_and_payloads(Result),
  HashId = hashids:encode(HashCtx, length(EventPayloads) + 1),
  NewEventPayload = ["event: ", get_event(Event), "\n",  "id: ", HashId, "\n",  "data: ", Data, "\n\n"],
  NewEventPayloads = EventPayloads ++ [NewEventPayload],
  set_payloads(Channel, HashCtx, NewEventPayloads),
  NewEventPayload.

% Retrieve all the payloads created after the passed LastEventId
% Note: There is a max of ?MAX_PAYLOADS_STORED_PER_CHANNEL stored per channel
get_payloads_after(Channel, LastEventId) ->
  {HashCtx, EventPayloads} = get_payloads(Channel),
  case HashCtx of
    undefined ->
      [];
    _ ->
      get_last_event_payloads(hashids:decode(HashCtx, binary_to_list(LastEventId)), EventPayloads)
  end.

get_last_event_payloads([LastEventPayloadIdx], EventPayloads) ->
  lists:sublist(EventPayloads, LastEventPayloadIdx, ?MAX_PAYLOADS_STORED_PER_CHANNEL);

get_last_event_payloads([], _) ->
  [].

get_event(<<"">>) ->
  <<"message">>;

get_event(Event) ->
  Event.

get_hash_ctx_and_payloads(Result) ->
  {HashCtx, EventPayloads} = Result,
  NumEventPayloads = length(EventPayloads),
  case (NumEventPayloads >= ?MAX_PAYLOADS_STORED_PER_CHANNEL) or (NumEventPayloads =< 0) of
    true ->
      Salt = integer_to_list(get_timestamp()),
      NewHashCtx = hashids:new([{salt, Salt}, {min_hash_length, ?HASH_MIN_LENGTH}]),
      {NewHashCtx, []};
    false -> 
      {HashCtx, EventPayloads}
  end.

get_payloads(Channel) ->
  get_payload_results(ets:lookup(channels, Channel)).

get_payload_results([{_, Result}]) ->
  Result;

get_payload_results([]) ->
  {undefined, []}.

set_payloads(Channel, HashCtx, Payloads) ->
  ets:insert(channels, {Channel, {HashCtx, Payloads}}).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).