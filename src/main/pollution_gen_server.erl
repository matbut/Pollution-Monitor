%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 12:06
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behavior(gen_server).
-author("mateusz").

%% API
-export([start/0,start_link/0,stop/0, init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([crash/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getOverLimit/2]).
-export([storeData/1]).

%% START %%
start_link() -> gen_server:start_link({local,pollution_gen_server},?MODULE,0,[]).
start() -> gen_server:start({local,pollution_gen_server},?MODULE,0,[]).

%% INTERFACE %%
stop() -> gen_server:call(pollution_gen_server,terminate).
crash() -> gen_server:cast(pollution_gen_server,crash).
addStation(Name,Cords) -> gen_server:call(pollution_gen_server,{addStation,[Name,Cords]}).
addValue(Id,Datetime,Type,Val) -> gen_server:call(pollution_gen_server,{addValue,[Id,Datetime,Type,Val]}).
removeValue(Id,Datetime,Type) -> gen_server:call(pollution_gen_server,{removeValue,[Id,Datetime,Type]}).
getOneValue(Id,Datetime,Type) -> gen_server:call(pollution_gen_server,{getOneValue,[Id,Datetime,Type]}).
getStationMean(Id,Type) -> gen_server:call(pollution_gen_server,{getStationMean,[Id,Type]}).
getDailyMean(Date,Type) -> gen_server:call(pollution_gen_server,{getDailyMean,[Date,Type]}).
getOverLimit(Date,Hour) -> gen_server:call(pollution_gen_server,{getOverLimit,[Date,Hour]}).

init(_State) ->
  case whereis(storeData) of
    undefined ->
      pollution:createMonitor();
    Pid ->
      Pid ! {self(),getData},
      receive
        {ok,Monitor} -> {ok,Monitor}
      end
  end.
%% HANDLE %%
handle_call(terminate, _Pid, State) -> {stop, normal, ok, State};
handle_call({Atom,Args}, Pid, State) when is_atom(Atom) -> react(Pid,Atom,Args,State).
handle_cast(crash,_State) -> 5/0.

react(_Pid,Atom,Args,State) ->
  try apply(pollution,Atom,Args++[State]) of
    {ok,Value} when is_number(Value) ->
      {reply,{ok,Value},State};
    {ok,NewState} ->
      {reply,ok,NewState};
    {error,Error} ->
      {reply,{error,Error},State}
  catch
    error:Error ->
      {reply,{error, Error},State}
  end.

handle_info(_Info, State) ->
  {reply,{error, "Illegal message"},State}.

terminate(normal,_State) ->
  ok;
terminate(_Other,State) ->
  register (storeData, spawn(?MODULE, storeData, [State])),
  ok.

storeData(Monitor) ->
  receive
    {Pid,getData} -> Pid ! {ok,Monitor}
  end.

code_change(_OldVsn,_State,_Extra) ->
  erlang:error(not_implemented).