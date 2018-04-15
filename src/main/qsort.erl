%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2018 11:32
%%%-------------------------------------------------------------------
-module(qsort).
-author("mateusz").

%% API
-export([qs/1,randomElems/3,compareSpeeds/3]).

lessThan(List, Arg) ->
  lists:filter(fun (X) -> X<Arg end,List).

grtEqThan(List, Arg) ->
  [X || X <- List, X>=Arg].

qs([Pivot|Tail]) ->
  qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot));
qs(Element) -> Element;
qs([]) -> [].

randomElems(N,Min,Max) ->
  [rand:uniform(Max-Min)+Min || X <- lists:seq(1,N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1,_}=timer:tc(Fun1,[List]),
  {Time2,_}=timer:tc(Fun2,[List]),
  {Time1,Time2}.