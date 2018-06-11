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
%-export([addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getOverLimit/3]).

connect(ConnectString) ->
  odbc:start(),
  odbc:connect(ConnectString,[]).

disconnect(Conn) ->
  odbc:disconnect(Conn).

containsStation(Cords={CordX,CordY},Name,Conn) ->
  Querry = string_format("SELECT * FROM Stations WHERE Name='~s' AND Coords.Lat = ~p AND Coords.Long=~p",[Name,CordX,CordY]),
  odbc:select_count(Conn,Querry).

string_format(Pattern, Values) ->
  lists:flatten(io_lib:format(Pattern, Values)).