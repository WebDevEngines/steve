-module(steve_channel).
-export([init/0, send_event/3, get_payloads_after/2]).
-define(HASH_MIN_LENGTH, os:getenv("HASH_MIN_LENGTH", 16)).
-define(MAX_PAYLOADS_STORED_PER_CHANNEL, os:getenv("MAX_PAYLOADS_STORED_PER_CHANNEL", 1000)).

init() ->
  ets:new(channels, [named_table, set, public]),
  ets:new(payloads, [named_table, set, public]),
  ok.

verify([{_, ChannelPid}]) ->
  case is_process_alive(ChannelPid) of
    false ->
      spawn(fun() -> loop() end);
    true ->
      ChannelPid
  end;

verify([]) ->
  spawn(fun() -> loop() end);

verify(Channel) ->
  ChannelPid = verify(ets:lookup(channels, Channel)),
  ets:insert(channels, {Channel, ChannelPid}),
  ChannelPid.

loop() ->
  receive
    {broadcast, Channel, Event, Data} ->
      StreamPids = steve_stream:get_pids(Channel),
      Payload = create_payload(Channel, Event, Data),
      stream_event(StreamPids, Payload)      
  end,
  loop().

stream_event([H|T], Payload) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, Payload};
    false ->
      steve_stream:remove_pid(StreamPid)
  end,
  stream_event(T, Payload);

stream_event([], _) ->
  ok.

send_event(Channel, Event, Data) ->
  ChannelPid = verify(Channel),
  ChannelPid ! {broadcast, Channel, Event, Data}.

create_payload(Channel, Event, Data) ->
  Result = get_payloads(Channel),
  {HashCtx, EventPayloads} = get_hash_ctx_and_payloads(Result),
  HashId = hashids:encode(HashCtx, length(EventPayloads) + 1),
  NewEventPayload = ["event: ", nvl_event(Event), "\n",  
                     "id: ", HashId, "\n",  
                     "data: ", Data, "\n\n"],
  NewEventPayloads = EventPayloads ++ [NewEventPayload],
  set_payloads(Channel, HashCtx, NewEventPayloads),
  NewEventPayload.

get_payloads_after(Channel, EventId) ->
  {HashCtx, EventPayloads} = get_payloads(Channel),
  case HashCtx of
    undefined ->
      [];
    _ ->
      get_last_event_payloads(
        hashids:decode(HashCtx, binary_to_list(EventId)),
        EventPayloads
      )
  end.

get_last_event_payloads([LastEventPayloadIdx], EventPayloads) ->
  lists:sublist(EventPayloads, LastEventPayloadIdx, ?MAX_PAYLOADS_STORED_PER_CHANNEL);

get_last_event_payloads([], _) ->
  [].

nvl_event(<<"">>) ->
  <<"message">>;

nvl_event(Event) ->
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
  get_payload_results(ets:lookup(payloads, Channel)).

get_payload_results([{_, Result}]) ->
  Result;

get_payload_results([]) ->
  {undefined, []}.

set_payloads(Channel, HashCtx, Payloads) ->
  ets:insert(payloads, {Channel, {HashCtx, Payloads}}).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).