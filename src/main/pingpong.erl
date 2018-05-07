%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:26
%%%-------------------------------------------------------------------
-module(pingpong).
-author("mateusz").

%% API
-export([start/0,stop/0,play/1]).


start() ->
  register(ping,spawn(fun ping_loop/0)),
  register(pong,spawn(fun pong_loop/0)).

play(N) -> ping ! N.

ping_loop() ->
  receive
    0 ->
      io:format("Ping recived 0~n"),
      ping_loop();
    stop -> term();
    N ->
      io:format("Ping recived ~B~n",[N]),
      timer:sleep(500),
      pong ! (N-1),
      ping_loop()
  after 20000 ->
    term()
  end.

pong_loop() ->
  receive
    0 ->
      io:format("Pong recived 0~n"),
      ping_loop();
    stop -> term();
    N ->
      io:format("Pong recived ~B~n",[N]),
      timer:sleep(500),
      ping ! (N-1),
      pong_loop()
  after 20000 ->
    term()
  end.

term() ->
  io:format("End of game"),
  ok.

stop() ->
  ping ! stop,
  pong ! stop.