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
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getOverLimit/3]).

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

createMonitor() ->
  {ok,#monitor{}}.

addStation(NewName,NewCords={NewCord_x,NewCord_y},Monitor)
  when is_number(NewCord_x), is_number(NewCord_y)
  %, 32=<Char, Char=<126
  ->

  case containsVal(NewCords,Monitor#monitor.stations) or maps:is_key(NewName,Monitor#monitor.stations) of
    false -> {ok,Monitor#monitor{stations = (Monitor#monitor.stations)#{NewName => NewCords}}};
    %true ->  erlang:error(?ADD_STATION_ERROR_MES)
    true ->  {error,"Monitor contains station with same name or same cords"}
  end;
  %false = containsVal(NewCords,Monitor#monitor.stations),
  %false = maps:is_key(NewName,Monitor#monitor.stations),
  %Monitor#monitor{stations = (Monitor#monitor.stations)#{NewName => NewCords}}.

addStation(_,_,_) -> {error,"Illegal cords format"}.


addValue(Id,Datetime={{Year,Month,Day},{Hour,Minute,Second}},Type,Val,Monitor)
  when
  0=<Year, 1=<Month, Month=<12, 1=<Day, Day=<31, 0=<Hour, Hour=<23, 0=<Minute, Minute=<60,0=<Second, Second=<59,
  is_number(Val) %,is_list(Type) ,
  ->

  case getStationCords(Id,Monitor) of
    {ok,Cords} ->
      InsertedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},

      case maps:is_key(InsertedKey,Monitor#monitor.values) of
        false -> {ok,Monitor#monitor{values = (Monitor#monitor.values)#{InsertedKey => Val}}};
        %true -> erlang:error(?ADD_VALUE_ERROR_MES)
        true -> {error,"Monitor contains value with same station id, date and type"}
      end;
    Error -> Error
  end;
  %InsertedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
  %false = maps:is_key(InsertedKey,Monitor#monitor.values),
  %Monitor#monitor{values = (Monitor#monitor.values)#{InsertedKey => Val}}.

addValue(_,_,_,_,_) -> {error,"Illegal datetime or value format"}.

removeValue(Id,Datetime,Type,Monitor) ->
  case getStationCords(Id,Monitor) of
    {ok,Cords} ->
      RemovedKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
      {ok,Monitor#monitor{values = maps:remove(RemovedKey,Monitor#monitor.values)}};
    Error -> Error
  end.

getOneValue(Id,Datetime,Type,Monitor) ->
  case getStationCords(Id,Monitor) of
    {ok,Cords} ->
      GetKey = #metaData{cords=Cords,datetime=Datetime,type=Type},
      {ok,maps:get(GetKey, Monitor#monitor.values)};
    Error -> Error
  end.

getStationMean(Id,GetType,Monitor) ->
  case getStationCords(Id,Monitor) of
    {ok,GetCords} ->
      {Sum,Num}=maps:fold(fun (#metaData{cords=Cords,type=Type},Val,{Acc,Num})
        when Cords==GetCords, Type==GetType ->
        {Val+Acc,Num+1}; (_,_,{Acc,Num}) -> {Acc,Num} end,{0,0},Monitor#monitor.values),
      {ok,Sum / Num};
    Error -> Error
  end.

getDailyMean(GetDate,GetType,Monitor) ->

  {Sum,Num}=maps:fold(fun
    (#metaData{datetime = {Date,{_,_,_}},type=Type},Val,{Acc,Num})
      when Type==GetType, Date==GetDate->
        {Val+Acc,Num+1};
    (_,_,{Acc,Num}) -> {Acc,Num}
  end,{0,0},Monitor#monitor.values),
  {ok,Sum / Num}.

getOverLimit(GetDate,GetHour,Monitor) ->
  {ok,maps:fold(fun
    (#metaData{type=Type, datetime={Date,{Hour,_,_}}},Val,Acc)
      when ((Type==?PM10_NAME) and (Val > ?PM10_NORM)) or ((Type==?PM25_NAME) and (Val > ?PM25_NORM)),
      Date==GetDate, Hour==GetHour->
        Acc+1;
    (_,_,Acc) -> Acc
  end,0,Monitor#monitor.values)}.

getStationCords(Cords={CordX,CordY},Monitor)
  when is_number(CordX), is_number(CordY) ->
  case containsVal(Cords,Monitor#monitor.stations) of
    true -> {ok,Cords};
    false -> {error,"Unknown station cords"}
  end;

getStationCords(Name,Monitor) ->
  case maps:is_key(Name,Monitor#monitor.stations) of
    true -> {ok,maps:get(Name,Monitor#monitor.stations)};
    false -> {error,"Unknown station name"}
  end.
  %maps:get(Name,Monitor#monitor.stations,{error,"Unknown station name"}).


containsVal(PrototypeVal,Map) ->
  maps:size(maps:filter(fun (_,Val) when Val==PrototypeVal -> true; (_,_) -> false end,Map)) > 0.