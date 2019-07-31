-module(steve_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(API_NUM_PROCS, os:getenv("API_NUM_PROCS", 5)).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [],
  {ok, {{one_for_one, 1, ?API_NUM_PROCS}, Procs}}.
