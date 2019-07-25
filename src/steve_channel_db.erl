-module(steve_channel_db).

-export([init/0, get_payloads/1, set_payloads/2]).

init() ->
  ets:new(channels, [named_table, set, public]),
  ok.

get_payloads(Channel) ->
  handle_lookup_result(ets:lookup(channels, Channel)).

handle_lookup_result([{_, Result}]) ->
  Result;

handle_lookup_result([]) ->
  [].

set_payloads(Channel, Payloads) ->
  ets:insert(channels, {Channel, Payloads}).