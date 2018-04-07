%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2018 12:32
%%%-------------------------------------------------------------------
-module(pollution).
-author("mateusz").

%% API
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3]).

-import(calendar,[local_time/0]).

-record(metaData, {
  cords,
  datetime = local_time(),
  type
}).

-record(monitor,{
  stations=#{},
  values=#{}
}).

%stations=#{"name" => {cord_x,cord_y}}
%values=#{metaData => val}

%Czy trzeba sprawdzac rekordy jak sie ich uzywa?

createMonitor() ->
  #monitor{}.

addStation(Monitor,NewName=[Char | _],NewCords={NewCord_x,NewCord_y})
  when is_number(NewCord_x), is_number(NewCord_y), 32=<Char, Char=<126 ->

  false = containsVal(NewCords,Monitor#monitor.stations),
  false = maps:is_key(NewName,Monitor#monitor.stations),
  Monitor#monitor{stations = (Monitor#monitor.stations)#{NewName => NewCords}}.


addValue(Monitor,Id,Datetime={{Year,Month,Day},{Hour,Minute,Second}},Type,Val)
  when 0=<Year, 1=<Month, Month=<12, 1=<Day, Day=<31, 0=<Hour, Hour=<23, 0=<Minute, Minute=<59,0=<Second, Second=<59, is_list(Type)->

  Cords=getStationCords(Monitor,Id),

  InsertedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  false = maps:is_key(InsertedKey,Monitor#monitor.values),
  Monitor#monitor{values = (Monitor#monitor.values)#{InsertedKey => Val}}.

removeValue(Monitor,Id,Datetime,Type) ->

  Cords=getStationCords(Monitor,Id),

  RemovedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  maps:remove(RemovedKey,Monitor#monitor.values).

getOneValue(Monitor,Id,Datetime,Type) ->

  Cords=getStationCords(Monitor,Id),
  GetKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  maps:get(GetKey, Monitor#monitor.values).

getStationMean(Monitor,Id,GetType) ->

  GetCords=getStationCords(Monitor,Id),

  {Sum,Num}=maps:fold(fun (#metaData{cords=Cords,type=Type},Val,{Acc,Num})
    when Cords==GetCords, Type==GetType ->
    {Val+Acc,Num+1}; (_,_,{Acc,Num}) -> {Acc,Num} end,{0,0},Monitor#monitor.values),
  Sum / Num.

getDailyMean(Monitor,GetDate,GetType) ->

  {Sum,Num}=maps:fold(fun (#metaData{datetime = {Date,{_,_,_}},type=Type},Val,{Acc,Num})
    when Type==GetType, Date==GetDate->
    {Val+Acc,Num+1}; (_,_,{Acc,Num}) -> {Acc,Num} end,{0,0},Monitor#monitor.values),
  Sum / Num.

getStationCords(Monitor,Cords={CordX,CordY})
  when is_number(CordX), is_number(CordY) ->
  true=containsVal(Cords,Monitor#monitor.stations),
  Cords;

getStationCords(Monitor,Name) ->
  maps:get(Name,Monitor#monitor.stations).

containsVal(PrototypeVal,Map) ->
  maps:size(maps:filter(fun (_,Val) when Val==PrototypeVal -> true; (_,_) -> false end,Map)) > 0.