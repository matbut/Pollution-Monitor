%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2018 00:00
%%%-------------------------------------------------------------------
-module(pollutionTests).
-author("mateusz").
-compile([export_all, debug_info]).
-include_lib("eunit/include/eunit.hrl").

-define(ADD_STATION_ERROR_MES,"Monitor contains station with same name or same cords").
-define(ADD_VALUE_ERROR_MES,"Monitor contains value with same station id, date and type").


addStation_test_() ->
  {setup,
    fun addStationStart/0,
    fun (Monitor) ->
      [addStation_SameCords(Monitor),
        addStation_SameName(Monitor)
     ]
    end}.

addStationStart() ->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  pollution:addStation("Bulwarowa",{120,150},Monitor1).

addStation_SameCords(Monitor) ->
  %[?_assertError(ADD_STATION_ERROR_MES,pollution:addStation("Slowackiego",{13.23,17.25},Monitor))].
  [?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Slowackiego",{13.23,17.25},Monitor)),
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Slowackiego",{120,150},Monitor))
  ].

addStation_SameName(Monitor) ->
  %[?_assertError(ADD_STATION_ERROR_MES,pollution:addStation("Krasickiego",{21.16,17.25},Monitor))].
  [?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Krasickiego",{21.16,17.25},Monitor)),
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Bulwarowa",{17,150},Monitor))].

