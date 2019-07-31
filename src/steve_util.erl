-module(steve_util).
-export([map_diff/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Diff passed maps
%
% TODO
% - Handle list changes properly i.e. partial update instead of swap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_diff(OldMap, NewMap) ->
  map_diff(OldMap, NewMap, [], []).

map_diff(OldMap, NewMap, Path, Results) ->
  NewMapKeys = maps:keys(NewMap),
  OldMapKeys = maps:keys(OldMap),
  Keys = sets:to_list(sets:from_list(NewMapKeys ++ OldMapKeys)),
  check_keys(Keys, NewMap, OldMap, Path, Results).

check_keys([H|T], NewMap, OldMap, Path, Results) ->
  NewPath = Path ++ [H],
  NewResults = Results ++ check_key(H, NewMap, OldMap, NewPath),
  check_keys(T, NewMap, OldMap, Path, NewResults);

check_keys([], _, _, _, Results) ->
  Results.

get_val(Key, Map) ->
  case maps:is_key(Key, Map) of
    true ->
      maps:get(Key, Map);
    false ->
      {badkey, Key}
  end.

check_key(Key, NewMap, OldMap, Path) ->
  NewValue = get_val(Key, NewMap),
  OldValue = get_val(Key, OldMap),

  compare_values(Path, NewValue, OldValue).

compare_values(Path, NewValue, {badkey, _}) ->
  [{add, Path, NewValue}];

compare_values(Path, {badkey, _}, _) ->
  [{remove, Path}];

compare_values(Path, NewValue, OldValue) when NewValue =/= OldValue ->
  case is_map(NewValue) and is_map(OldValue) of
    true ->
      map_diff(OldValue, NewValue, Path, []);
    false ->
      [{replace, Path, NewValue}]
  end;

compare_values(_, _, _) ->
  [{}].
