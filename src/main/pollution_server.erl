%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 10:02
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("mateusz").

%% API
-export([start/0,stop/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getOverLimit/2]).

-export([init/0]).

start() ->
  register (pollution_server, spawn_link(?MODULE, init, [])),
  ok.

stop() ->
  call(stop,[]).

addStation(Name,Cords) ->
  call(addStation,[Name,Cords]).

addValue(Id,Datetime,Type,Val) ->
  call(addValue,[Id,Datetime,Type,Val]).

removeValue(Id,Datetime,Type) ->
  call(removeValue,[Id,Datetime,Type]).

getOneValue(Id,Datetime,Type) ->
  call(getOneValue,[Id,Datetime,Type]).

getStationMean(Id,Type) ->
  call(getStationMean,[Id,Type]).

getDailyMean(Date,Type) ->
  call(getDailyMean,[Date,Type]).

getOverLimit(Date,Hour) ->
  call(getOverLimit,[Date,Hour]).

init() ->
  {ok,State} = pollution:createMonitor(),
  loop(State).

loop(State) ->
  receive
    {request,Pid,stop,[]} ->
      Pid ! {reply,terminate()};
    {request,Pid,Atom,Args} when is_atom(Atom)-> react(Pid,Atom,Args,State)
  end.

react(Pid,Atom,Args,State) ->

  try apply(pollution,Atom,Args++[State]) of
    {ok,Value} when is_number(Value) ->
      Pid ! {reply,{ok,Value}},
      loop(State);
    {ok,NewState} ->
      Pid ! {reply,ok},
      loop(NewState);
    {error,Error} ->
      Pid ! {reply,{error,Error}},
      loop(State)
  catch
    error:Error ->
      Pid ! {reply,{error, Error}},
      loop(State)
  end.




call(Request, Args) ->
  pollution_server ! {request,self(),Request, Args},
  receive
    {reply, Reply} -> Reply
  end.

terminate() ->
  ok.