%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 12:46
%%%-------------------------------------------------------------------
-module(pollution_gen_server_sup).
-behavior(supervisor).
-author("mateusz").

%% API
-export([start_link/0, init/1, stop/0]).


init(_Args) ->
  io:format(_Args),
  {ok, {
    {one_for_all, 2, 3},
    [ {pollution_gen_server,
      {pollution_gen_server, start_link, []},
      permanent, brutal_kill, worker, [var_server]}
    ]}
  }.

start_link() ->
  supervisor:start_link({local,pollution_gen_server_sup},pollution_gen_server_sup,[]).

stop() ->
  erlang:error(not_implemented).