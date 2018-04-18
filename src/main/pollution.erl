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
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getOverLimit/1]).

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
%Errors handling

-define(PM10_NAME,"PM10").
-define(PM25_NAME,"PM2.5").
-define(PM10_NORM,50).
-define(PM25_NORM,30).

-define(ADD_STATION_ERROR_MES,"Monitor contains station with same name or same cords").
-define(ADD_VALUE_ERROR_MES,"Monitor contains value with same station id, date and type").

createMonitor() ->
  #monitor{}.

addStation(NewName=[Char | _],NewCords={NewCord_x,NewCord_y},Monitor)
  when is_number(NewCord_x), is_number(NewCord_y), 32=<Char, Char=<126 ->

  case containsVal(NewCords,Monitor#monitor.stations) or maps:is_key(NewName,Monitor#monitor.stations) of
    false -> Monitor#monitor{stations = (Monitor#monitor.stations)#{NewName => NewCords}};
    %true ->  erlang:error(?ADD_STATION_ERROR_MES)
    true ->  {error,?ADD_STATION_ERROR_MES}
  end.
  %false = containsVal(NewCords,Monitor#monitor.stations),
  %false = maps:is_key(NewName,Monitor#monitor.stations),
  %Monitor#monitor{stations = (Monitor#monitor.stations)#{NewName => NewCords}}.

addValue(Id,Datetime={{Year,Month,Day},{Hour,Minute,Second}},Type,Val,Monitor)
  when 0=<Year, 1=<Month, Month=<12, 1=<Day, Day=<31, 0=<Hour, Hour=<23, 0=<Minute, Minute=<59,0=<Second, Second=<59, is_list(Type)->

  Cords=getStationCords(Id,Monitor),
  InsertedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},

  case maps:is_key(InsertedKey,Monitor#monitor.values) of
    false -> Monitor#monitor{values = (Monitor#monitor.values)#{InsertedKey => Val}};
    %true -> erlang:error(?ADD_VALUE_ERROR_MES)
    true -> {error,?ADD_VALUE_ERROR_MES}
  end.
  %InsertedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  %false = maps:is_key(InsertedKey,Monitor#monitor.values),
  %Monitor#monitor{values = (Monitor#monitor.values)#{InsertedKey => Val}}.

removeValue(Id,Datetime,Type,Monitor) ->

  Cords=getStationCords(Id,Monitor),

  RemovedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  Monitor#monitor{values = maps:remove(RemovedKey,Monitor#monitor.values)}.

getOneValue(Id,Datetime,Type,Monitor) ->

  Cords=getStationCords(Id,Monitor),
  GetKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  maps:get(GetKey, Monitor#monitor.values).

getStationMean(Id,GetType,Monitor) ->

  GetCords=getStationCords(Id,Monitor),

  {Sum,Num}=maps:fold(fun (#metaData{cords=Cords,type=Type},Val,{Acc,Num})
    when Cords==GetCords, Type==GetType ->
    {Val+Acc,Num+1}; (_,_,{Acc,Num}) -> {Acc,Num} end,{0,0},Monitor#monitor.values),
  Sum / Num.

getDailyMean(GetDate,GetType,Monitor) ->

  {Sum,Num}=maps:fold(fun (#metaData{datetime = {Date,{_,_,_}},type=Type},Val,{Acc,Num})
    when Type==GetType, Date==GetDate->
    {Val+Acc,Num+1}; (_,_,{Acc,Num}) -> {Acc,Num} end,{0,0},Monitor#monitor.values),
  Sum / Num.

getOverLimit(Monitor) ->
  maps:fold(fun
    (#metaData{type=Type},Val,Acc)
      when ((Type==?PM10_NAME) and (Val > ?PM10_NORM)) or ((Type==?PM25_NAME) and (Val > ?PM25_NORM)) ->
        Acc+1;
    (_,_,Acc) -> Acc
  end,0,Monitor#monitor.values).

getStationCords(Cords={CordX,CordY},Monitor)
  when is_number(CordX), is_number(CordY) ->
  true=containsVal(Cords,Monitor#monitor.stations),
  Cords;

getStationCords(Name,Monitor) ->
  maps:get(Name,Monitor#monitor.stations).

containsVal(PrototypeVal,Map) ->
  maps:size(maps:filter(fun (_,Val) when Val==PrototypeVal -> true; (_,_) -> false end,Map)) > 0.