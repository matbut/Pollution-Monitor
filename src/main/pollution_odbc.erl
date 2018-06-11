%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2018 11:05
%%%-------------------------------------------------------------------
-module(pollution_odbc).
-author("mateusz").

%% API
-export([connect/1,disconnect/1,containsStation/3]).
-export([addStation/3]).%,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getOverLimit/3]).
-export([getStationCords/2]).

connect(ConnectString) ->
  odbc:start(),
  odbc:connect(ConnectString,[]).

disconnect(Conn) ->
  odbc:disconnect(Conn).

addStation(Coords={CordX,CordY},Name,Conn) ->
  case containsStation(Coords,Name,Conn) of
    {ok,false} ->
      Query = string_format("INSERT INTO Stations VALUES('~s', geography::Point(~p, ~p, 4326))",[Name,CordX,CordY]),
      odbc:sql_query(Conn,Query);
    _ -> {error,"Station already exists"}
  end.

addValue(Id,Datetime={{Year,Month,Day},{Hour,Minute,Second}},Type,Val,Monitor)
  when
  0=<Year, 1=<Month, Month=<12, 1=<Day, Day=<31, 0=<Hour, Hour=<23, 0=<Minute, Minute=<60,0=<Second, Second=<59,
  is_number(Val) %,is_list(Type) ,
  ->

  case getStationCords(Id,Monitor) of
    {ok,Cords} ->
      Query = string_format("INSERT INTO Measurements VALUES('~s', geography::Point(~p, ~p, 4326))",[Name,CordX,CordY]),
      ;
    Error -> Error
  end;
addValue(_,_,_,_,_) -> {error,"Illegal datetime or value format"}.

containsStation({CordX,CordY},Name,Conn) ->
  Query = string_format("SELECT * FROM Stations WHERE Name='~s' OR (Coords.Lat = ~p AND Coords.Long=~p)",[Name,CordX,CordY]),
  case odbc:select_count(Conn,Query) of
    {ok,0} -> {ok,false};
    {ok,_} -> {ok,true};
    {error,Reason} -> erlang:error(Reason)
  end.

getStationCords(Cords={CordX,CordY},Conn) ->
  Query = string_format("SELECT * FROM Stations WHERE Coords.Lat = ~p AND Coords.Long=~p",[CordX,CordY]),
  case odbc:select_count(Conn,Query) of
    {ok,1} -> {ok,Cords};
    {ok,0} -> {error,"Station with given co-ordinates doesn't exist"};
    {error,Reason} -> erlang:error(Reason)
  end;

getStationCords(Name,Conn) ->
  Query = string_format("SELECT Coords.Lat, Coords.Long FROM Stations WHERE Name = '~s'",[Name]),
  case odbc:select_count(Conn,Query) of
    {ok,1} ->
      {_,_,[Coords]} = odbc:first(Conn),
      {ok,Coords};
    {ok,0} -> {error,"Station with given name doesn't exist"};
    {error,Reason} -> erlang:error(Reason)
  end.

string_format(Pattern, Values) ->
  lists:flatten(io_lib:format(Pattern, Values)).