-module(steve_channel).
-export([init/0, add_stream/2, send_event/3, num_streams/0, create_payload/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the channel module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  quickrand:seed(),
  ets:new(channels, [named_table, set, public]),
  ets:new(streams, [named_table, set, public]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Send event to all clients connected to the channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_event(Channel, Event, Data) ->
  ChannelPid = verify_available(Channel),
  ChannelPid ! {send_event, Channel, Event, Data}.

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
      Payload = create_payload(Event, Data),
      stream_event(StreamPids, Payload)
  end,
  message_loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stream event to connected stream(s)
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

create_payload(Data) ->
  create_payload("message", Data).

create_payload(Event, Data) ->
  ["event: ", Event, "\n",
   "id: ", get_id(), "\n",
   "data: ", Data, "\n\n"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get a unique id for the payload
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_id() ->
  uuid:uuid_to_string(uuid:get_v4_urandom()).

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
