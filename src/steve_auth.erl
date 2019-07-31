-module(steve_auth).
-export([init/0, is_authorized/2]).
-define(AUTHORIZATION_WEBHOOK, os:getenv("AUTHORIZATION_WEBHOOK", undefined)).
-define(AUTHORIZATION_WEBHOOK_TIMEOUT, os:getenv("AUTHORIZATION_WEBHOOK_TIMEOUT", 2500)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check passed AuthToken and DocumentId to see if it is authorized by
% calling an external user defined webhook
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  application:ensure_all_started(gun),
  ok.

is_authorized(undefined, _DocumentId) ->
  true;

is_authorized(AuthToken, DocumentId) ->
  case ?AUTHORIZATION_WEBHOOK of
    undefined ->
      true;
    _ ->
      {uri, _, _, Host, Port, Path, _, _, _} = uri:from_string(?AUTHORIZATION_WEBHOOK),
      {ok, ConnPid} = gun:open(binary_to_list(Host), Port),
      {ok, _} = gun:await_up(ConnPid),
      Payload = jsone:encode(#{<<"authorization">> => AuthToken,
                               <<"documentId">> => DocumentId}),
      StreamRef = gun:post(ConnPid, Path, [], Payload),
      receive
          {gun_response, ConnPid, StreamRef, _, Status, _} ->
              case Status of
                200 ->
                  true;
                _ ->
                  false
              end;
          {'DOWN', _, process, _, _} ->
              false
      after ?AUTHORIZATION_WEBHOOK_TIMEOUT ->
          false
      end
    end.
