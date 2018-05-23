%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 11:29
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("mateusz").

%% API
-export([start/0,stop/0]).
-export([init/0]).

start() ->
  register (pollution_server_sup, spawn_link(?MODULE, init, [])),
  ok.

init() ->
  process_flag(trap_exit, true),
  pollution_server:start_link(),
  loop().

stop() ->
  pollution_server_sup ! stop.

loop() ->
  %pollution_server:start_link(),
  receive
    {'EXIT', _Pid, normal} -> io:format("Server exit normal "), terminate();
    {'EXIT', _Pid, _Reason} -> pollution_server:start_link(), io:format("Restart server after crach. "), loop();
    stop -> ok=pollution_server:stop(), io:format("Server supervisor stopped "), loop() %waiting for EXIT from server
  end.

terminate() ->
  ok.