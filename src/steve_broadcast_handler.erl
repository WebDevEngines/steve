-module(steve_broadcast_handler).
-export([init/2, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the broadcast handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  #{channel := Channel, event := Event, data := Data} = cowboy_req:match_qs(
    [channel, {event, [], <<"message">>}, data], Req),

  % Send data to channel
  steve_channel:send_event(Channel, Event, Data),

  % Send response back to the client
  Resp = cowboy_req:reply(
    202, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, <<"">>, Req),

  {ok, Resp, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cleanup after the handler exits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _Req, _State) ->
    ok.
