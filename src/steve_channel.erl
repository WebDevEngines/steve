-module(steve_channel).
-export([init/0, add_stream/2, send_event/3,
  get_payloads_after/2, num_streams/0]).
-define(HASH_MIN_LENGTH, os:getenv("HASH_MIN_LENGTH", 16)).
-define(MAX_PAYLOADS_STORED_PER_CHANNEL,
  os:getenv("MAX_PAYLOADS_STORED_PER_CHANNEL", 1000)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the channel module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  ets:new(channels, [named_table, set, public]),
  ets:new(payloads, [named_table, set, public]),
  ets:new(streams, [named_table, set, public]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Send event to all clients connected to the channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_event(Channel, Event, Data) ->
  ChannelPid = verify_available(Channel),
  ChannelPid ! {send_event, Channel, Event, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Retrieve channel payloads created after the passed event id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_payloads_after(Channel, EventId) ->
  {EventIdHashCtx, EventPayloads} = get_payloads(Channel),
  case EventIdHashCtx of
    undefined ->
      [];
    _ ->
      get_last_event_payloads(
        hashids:decode(EventIdHashCtx, binary_to_list(EventId)),
        EventPayloads
      )
  end.

get_last_event_payloads([LastEventPayloadIdx], EventPayloads) ->
  lists:sublist(
    EventPayloads,
    LastEventPayloadIdx,
    ?MAX_PAYLOADS_STORED_PER_CHANNEL
  );

get_last_event_payloads([], _) ->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verify that the channel has a running process to accept broadcast requests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_available([{_, ChannelPid}]) ->
  case is_process_alive(ChannelPid) of
    false ->
      spawn(fun() -> message_loop() end);
    true ->
      ChannelPid
  end;

verify_available([]) ->
  spawn(fun() -> message_loop() end);

verify_available(Channel) ->
  ChannelPid = verify_available(ets:lookup(channels, Channel)),
  ets:insert(channels, {Channel, ChannelPid}),
  ChannelPid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verify that the channel has a running process to accept broadcast requests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message_loop() ->
  receive
    {send_event, Channel, Event, Data} ->
      StreamPids = get_streams(Channel),
      Payload = create_payload(Channel, Event, Data),
      stream_event(StreamPids, Payload)
  end,
  message_loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stream event to connected streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stream_event([H|T], Payload) ->
  {StreamPid, _} = H,
  case is_process_alive(StreamPid) of
    true ->
      StreamPid ! {event, Payload};
    false ->
      remove_stream(StreamPid)
  end,
  stream_event(T, Payload);

stream_event([], _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a new payload to stream
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_payloads([]) ->
  get_payloads([{channel, {undefined, []}}]);

get_payloads([{_, {HashCtx, EventPayloads}}]) ->
  NumEventPayloads = length(EventPayloads),
  case (NumEventPayloads >= ?MAX_PAYLOADS_STORED_PER_CHANNEL) or
       (NumEventPayloads =< 0) of
    true ->
      NewHashCtx = hashids:new([
        {salt, integer_to_list(get_timestamp())},
        {min_hash_length, ?HASH_MIN_LENGTH}
      ]),
      {NewHashCtx, []};
    false ->
      {HashCtx, EventPayloads}
  end;

get_payloads(Channel) ->
  get_payloads(ets:lookup(payloads, Channel)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a new payload to stream
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_payload(Channel, Event, Data) ->
  {EventIdHashCtx, EventPayloads} = get_payloads(Channel),
  EventId = hashids:encode(EventIdHashCtx, length(EventPayloads) + 1),
  NewEventPayload = ["event: ", nvl_event(Event), "\n",
                     "id: ", EventId, "\n",
                     "data: ", Data, "\n\n"],
  NewEventPayloads = EventPayloads ++ [NewEventPayload],
  ets:insert(payloads, {Channel, {EventIdHashCtx, NewEventPayloads}}),
  NewEventPayload.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If an `event` is not passed then use `message`
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nvl_event(<<"">>) ->
  <<"message">>;

nvl_event(Event) ->
  Event.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the current timestamp in milliseconds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manage stream processes by channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_streams(Channel) ->
  ets:match_object(streams, {'_', Channel}).

add_stream(Pid, Channel) ->
  ets:insert(streams, {Pid, Channel}).

remove_stream(Pid) ->
  ets:delete(streams, Pid).

num_streams() ->
  ets:info(streams, size).
