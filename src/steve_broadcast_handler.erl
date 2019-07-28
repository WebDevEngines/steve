-module(steve_broadcast_handler).
-export([init/2, terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize the broadcast handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
  #{channel := Channel,
    event := Event,
    data := Data} = cowboy_req:match_qs([channel, {event, [], <<"message">>}, data], Req),

  % Send event to channel
  steve_channel:send_event(Channel, Event, Data),

  % Send response back to the client
  TextPlain = #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
  Resp = cowboy_req:reply(202, TextPlain, <<"">>, Req),

  {ok, Resp, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cleanup after the handler exits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _Req, _State) ->
    ok.
