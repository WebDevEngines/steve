-module(steve_broadcast_handler).

-export([init/2, handle/1, terminate/3]).

init(Req, State) ->
	Resp = handle(Req),
	{ok, Resp, State}.

% Broadcast the passed message to all the open streams
handle(Req) ->
  % Get the message from the query string
  #{msg := Msg} = cowboy_req:match_qs([msg], Req),
  % Send a message to all stream proceses
  send_messages(pg2:get_members(stream_processes), Msg),
  % Close the HTTP connection
  cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"Message Sent!">>,
    Req
  ).

% Send a message to each process in the process group
send_messages([H|T], Msg) ->
    H ! {event, ["msg: ", Msg, "\n\n"]},
    send_messages(T, Msg);
send_messages([], _) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.
