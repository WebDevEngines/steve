-module(steve_stream_handler).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, State) ->
    Resp = cowboy_req:stream_reply(200, Req),
    % Add the cowboy_loop process to a process group
    pg2:join(stream_processes, self()),
    {cowboy_loop, Resp, State}.

info(eof, Req, State) ->
  {stop, Req, State};

info({event, Data}, Req, State) ->
    cowboy_req:stream_body(Data, nofin, Req),
    {ok, Req, State};

info(_Msg, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
