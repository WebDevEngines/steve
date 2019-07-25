-module(steve_channel_router).

-export([start/0, get_payloads_after/2]).

-define(HASH_ID_SALT, "steve_hash_id_salt").
-define(HASH_MIN_LENGTH, 8).
-define(MAX_MESSAGES_STORED_PER_CHANNEL, 1000).

start() ->
  HashCtx = hashids:new([{salt, ?HASH_ID_SALT}, {min_hash_length, ?HASH_MIN_LENGTH}]),
  spawn(fun() -> channel_router_loop(HashCtx) end).

channel_router_loop(HashCtx) ->
  receive
    {broadcast, Channel, Event, Data} ->
      StreamPids = steve_stream_db:get_pids(Channel),
      Payload = create_event_payload(HashCtx, Channel, Event, Data),
      send_event(StreamPids, Payload),
      channel_router_loop(HashCtx)
  end.

send_event([H|T], Payload) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, Payload};
    false ->
      steve_stream_db:remove_pid(StreamPid)
  end,
  send_event(T, Payload);

send_event([], _) ->
  ok.

create_event_payload(HashCtx, Channel, Event, Data) ->
  EventPayloads = steve_channel_db:get_payloads(Channel),
  HashId = hashids:encode(HashCtx, length(EventPayloads) + 1),
  NewEventPayload = ["event: ", Event, "\n",  "id: ", HashId, "\n",  "data: ", Data, "\n\n"],
  NewEventPayloads = EventPayloads ++ [NewEventPayload],
  steve_channel_db:set_payloads(
    Channel, 
    lists:sublist(NewEventPayloads, ?MAX_MESSAGES_STORED_PER_CHANNEL)
  ),
  NewEventPayload.

get_payloads_after(Channel, HashId) ->
  HashCtx = hashids:new([{salt, ?HASH_ID_SALT}, {min_hash_length, ?HASH_MIN_LENGTH}]),
  EventPayloads = steve_channel_db:get_payloads(Channel),
  [LastEventPayloadIdx] = hashids:decode(HashCtx, binary_to_list(HashId)),
  lists:sublist(
    EventPayloads, 
    LastEventPayloadIdx, 
    ?MAX_MESSAGES_STORED_PER_CHANNEL
  ).