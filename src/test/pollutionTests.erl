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
        addStation_SameName(Monitor),
        addStations_IncorrectName(Monitor),
        addStations_IncorrectCords(Monitor)
     ]
    end}.

addStationStart() ->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  pollution:addStation("Bulwarowa",{120,150},Monitor1).

addStation_SameCords(Monitor) ->
  %[?_assertError(ADD_STATION_ERROR_MES,pollution:addStation("Slowackiego",{13.23,17.25},Monitor))].
  [
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Slowackiego",{13.23,17.25},Monitor)),
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Slowackiego",{120,150},Monitor))
  ].

addStation_SameName(Monitor) ->
  %[?_assertError(ADD_STATION_ERROR_MES,pollution:addStation("Krasickiego",{21.16,17.25},Monitor))].
  [
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Krasickiego",{21.16,17.25},Monitor)),
    ?_assertEqual({error,?ADD_STATION_ERROR_MES},pollution:addStation("Bulwarowa",{17,150},Monitor))
  ].

addStations_IncorrectName(Monitor) ->
  [
    ?_assertError(function_clause,pollution:addStation(bujaka,{133.5,31.4},Monitor)),
    ?_assertError(function_clause,pollution:addStation([15,65,66],{133.5,31.4},Monitor)),
    ?_assertError(function_clause,pollution:addStation(["Stacja","Bujaka"],{133.5,31.4},Monitor)),
    ?_assertError(function_clause,pollution:addStation({"Stacja","Bujaka"},{133.5,31.4},Monitor))
  ].

addStations_IncorrectCords(Monitor) ->
  [
    ?_assertError(function_clause,pollution:addStation("Bujaka",{five,21},Monitor)),
    ?_assertError(function_clause,pollution:addStation("Bujaka",{13.4,[14,153]},Monitor)),
    ?_assertError(function_clause,pollution:addStation("Bujaka",{13.4,{14,153}},Monitor))
  ].

addGetValue_test_() ->
  {setup,
    fun addGetValueStart/0,
    fun (Monitor) ->
      [
        addGetValues_Correct(Monitor),
        addValues_SameKey(Monitor),
        addValues_IncorrectStation(Monitor)
      ]
    end}.

addGetValueStart() ->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  Monitor2=pollution:addStation("Bulwarowa",{120,150},Monitor1),

  Monitor3=pollution:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",79,Monitor2),
  Monitor4=pollution:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",116,Monitor3),

  Monitor5=pollution:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",17,Monitor4),
  Monitor6=pollution:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor5),

  Monitor6.

addGetValues_Correct(Monitor) ->
  [
    ?_assertEqual(79,pollution:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertEqual(79,pollution:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertEqual(116,pollution:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM2.5",Monitor)),
    ?_assertEqual(116,pollution:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",Monitor)),
    ?_assertEqual(17,pollution:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertEqual(17,pollution:getOneValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertEqual(215,pollution:getOneValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",Monitor)),
    ?_assertEqual(215,pollution:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor))
  ].

addValues_SameKey(Monitor) ->
  [
    ?_assertEqual({error,?ADD_VALUE_ERROR_MES},pollution:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",195,Monitor)),
    ?_assertEqual({error,?ADD_VALUE_ERROR_MES},pollution:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",7,Monitor)),
    ?_assertEqual({error,?ADD_VALUE_ERROR_MES},pollution:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",17,Monitor)),
    ?_assertEqual({error,?ADD_VALUE_ERROR_MES},pollution:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor))
  ].

addValues_IncorrectStation(Monitor) ->
  [
    ?_assertError(_,pollution:addValue("Bujaka",{{1997,11,01},{11,15,27}},"PM10",195,Monitor)),
    ?_assertError({badkey,bujaka},pollution:addValue(bujaka,{{1997,11,01},{11,15,27}},"PM2.5",7,Monitor)),
    ?_assertError({badkey,["Stacja","Bujaka"]},pollution:addValue(["Stacja","Bujaka"],{{1997,11,01},{11,15,27}},"PM10",17,Monitor)),
    ?_assertError({badkey,{"Stacja","Bujaka"}},pollution:addValue({"Stacja","Bujaka"},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor)),

    ?_assertError(_,pollution:addValue({5,21},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor)),
    ?_assertError({badkey,{five,21}},pollution:addValue({five,21},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor)),
    ?_assertError({badkey,{13.4,[14,153]}},pollution:addValue({13.4,[14,153]},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor)),
    ?_assertError({badkey,{13.4,{14,153}}},pollution:addValue({13.4,{14,153}},{{1997,11,01},{12,15,27}},"PM2.5",215,Monitor))
  ].

removeValue_test_() ->
  {setup,
    fun addGetValueStart/0,
    fun (Monitor) ->
      [
        removeValue_Correct(Monitor),
        removeValue_Double(Monitor),
        removeValue_IncorrectKey(Monitor)
      ]
    end}.

removeValue_Correct(Monitor) ->
  Monitor1=pollution:removeValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor),
  Monitor2=pollution:removeValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",Monitor1),
  Monitor3=pollution:removeValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor2),
  Monitor4=pollution:removeValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor3),
  [
    ?_assertEqual(79,pollution:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertError(_,pollution:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor1)),
    ?_assertEqual(116,pollution:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",Monitor1)),
    ?_assertError(_,pollution:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10",Monitor2)),
    ?_assertEqual(17,pollution:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor2)),
    ?_assertError(_,pollution:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor3)),
    ?_assertEqual(215,pollution:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor3)),
    ?_assertError(_,pollution:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor4))
  ].

removeValue_Double(Monitor) ->
  Monitor1=pollution:removeValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor),
  Monitor2=pollution:removeValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",Monitor1),
  Monitor3=pollution:removeValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor2),
  Monitor4=pollution:removeValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor3),
  [
    ?_assertError(_,pollution:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",Monitor1)),
    ?_assertError(_,pollution:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10",Monitor2)),
    ?_assertError(_,pollution:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10",Monitor3)),
    ?_assertError(_,pollution:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5",Monitor4))
  ].

removeValue_IncorrectKey(Monitor) ->
  [
    ?_assertError({badkey,"Bujaka"},pollution:removeValue("Bujaka",{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertError({badkey,bujaka},pollution:removeValue(bujaka,{{1997,11,01},{11,15,27}},"PM2.5",Monitor)),
    ?_assertError({badkey,["Stacja","Bujaka"]},pollution:removeValue(["Stacja","Bujaka"],{{1997,11,01},{11,15,27}},"PM10",Monitor)),
    ?_assertError({badkey,{"Stacja","Bujaka"}},pollution:removeValue({"Stacja","Bujaka"},{{1997,11,01},{12,15,27}},"PM2.5",Monitor)),

    ?_assertError({badmatch,false},pollution:removeValue({5,21},{{1997,11,01},{12,15,27}},"PM2.5",Monitor)),
    ?_assertError({badkey,{five,21}},pollution:removeValue({five,21},{{1997,11,01},{12,15,27}},"PM2.5",Monitor)),
    ?_assertError({badkey,{13.4,[14,153]}},pollution:removeValue({13.4,[14,153]},{{1997,11,01},{12,15,27}},"PM2.5",Monitor)),
    ?_assertError({badkey,{13.4,{14,153}}},pollution:removeValue({13.4,{14,153}},{{1997,11,01},{12,15,27}},"PM2.5",Monitor))
  ].

getStationMean_test_() ->
  {setup,
    fun getStationMeanStart/0,
    fun (Monitor) ->
      [
        getStationMean_Correct(Monitor),
        getStationMean_IncorrectStation(Monitor)
      ]
    end}.

getStationMeanStart() ->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  Monitor2=pollution:addStation("Bulwarowa",{120,150},Monitor1),

  Monitor3=pollution:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",2000,Monitor2),
  Monitor4=pollution:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",3000,Monitor3),

  Monitor5=pollution:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",10,Monitor4),
  Monitor6=pollution:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",100,Monitor5),
  Monitor7=pollution:addValue("Bulwarowa",{{1997,11,01},{15,05,46}},"PM10",20,Monitor6),
  Monitor8=pollution:addValue({120,150},{{1997,11,01},{2,25,11}},"PM2.5",200,Monitor7),

  Monitor9=pollution:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",30,Monitor8),
  Monitor10=pollution:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",300,Monitor9),

  Monitor10.

getStationMean_Correct(Monitor) ->
  [
    ?_assertEqual(2000.0,pollution:getStationMean("Krasickiego","PM10",Monitor)),
    ?_assertEqual(2000.0,pollution:getStationMean({13.23,17.25},"PM10",Monitor)),
    ?_assertEqual(3000.0,pollution:getStationMean("Krasickiego","PM2.5",Monitor)),
    ?_assertEqual(3000.0,pollution:getStationMean({13.23,17.25},"PM2.5",Monitor)),

    ?_assertEqual(20.0,pollution:getStationMean("Bulwarowa","PM10",Monitor)),
    ?_assertEqual(20.0,pollution:getStationMean({120,150},"PM10",Monitor)),
    ?_assertEqual(200.0,pollution:getStationMean("Bulwarowa","PM2.5",Monitor)),
    ?_assertEqual(200.0,pollution:getStationMean({120,150},"PM2.5",Monitor))
  ].

getStationMean_IncorrectStation(Monitor) ->
  [
    ?_assertError({badkey,"Bujaka"},pollution:getStationMean("Bujaka","PM10",Monitor)),
    ?_assertError({badmatch,false},pollution:getStationMean({12.3,43.4},"PM10",Monitor)),
    ?_assertError(badarith,pollution:getStationMean("Bulwarowa","NO2",Monitor))
  ].

getDailyMean_test_() ->
  {setup,
    fun getDailyMeanStart/0,
    fun (Monitor) ->
      [
        getDailyMean_Correct(Monitor),
        getDailyMean_IncorrectStation(Monitor)
      ]
    end}.

getDailyMeanStart() ->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  Monitor2=pollution:addStation("Bulwarowa",{120,150},Monitor1),

  Monitor3=pollution:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",30,Monitor2),
  Monitor4=pollution:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",300,Monitor3),

  Monitor5=pollution:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",10,Monitor4),
  Monitor6=pollution:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",100,Monitor5),
  Monitor7=pollution:addValue("Bulwarowa",{{1997,11,01},{15,05,46}},"PM10",20,Monitor6),
  Monitor8=pollution:addValue({120,150},{{1997,11,01},{2,25,11}},"PM2.5",200,Monitor7),

  Monitor9=pollution:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",6000,Monitor8),
  Monitor10=pollution:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",3000,Monitor9),

  Monitor10.

getDailyMean_Correct(Monitor) ->
  [
    ?_assertEqual(20.0,pollution:getDailyMean({1997,11,01},"PM10",Monitor)),
    ?_assertEqual(200.0,pollution:getDailyMean({1997,11,01},"PM2.5",Monitor)),
    ?_assertEqual(6000.0,pollution:getDailyMean({1997,11,02},"PM10",Monitor)),
    ?_assertEqual(3000.0,pollution:getDailyMean({1998,11,01},"PM2.5",Monitor))
  ].

getDailyMean_IncorrectStation(Monitor) ->
  [
    ?_assertError({badkey,"Bujaka"},pollution:getStationMean("Bujaka","PM10",Monitor)),
    ?_assertError({badmatch,false},pollution:getStationMean({12.3,43.4},"PM10",Monitor)),
    ?_assertError(badarith,pollution:getStationMean("Bulwarowa","NO2",Monitor))
  ].

getOverLimit_test_() ->
  {setup,
    fun getOverLimitStart/0,
    fun (Monitor) ->
      [
        getOverLimit_Correct(Monitor)
      ]
    end}.

getOverLimitStart()->
  Monitor=pollution:createMonitor(),
  Monitor1=pollution:addStation("Krasickiego",{13.23,17.25},Monitor),
  Monitor2=pollution:addStation("Bulwarowa",{120,150},Monitor1),

  Monitor3=pollution:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",51,Monitor2),
  Monitor4=pollution:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",31,Monitor3),

  Monitor5=pollution:addValue("Krasickiego",{{1997,11,02},{11,15,27}},"PM10",50,Monitor4),
  Monitor6=pollution:addValue({13.23,17.25},{{1997,11,02},{11,15,27}},"PM2.5",30,Monitor5),

  Monitor7=pollution:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",50.3,Monitor6),
  Monitor8=pollution:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",30.2,Monitor7),
  Monitor9=pollution:addValue("Bulwarowa",{{1997,11,01},{15,05,46}},"PM10",49.7,Monitor8),
  Monitor10=pollution:addValue({120,150},{{1997,11,01},{2,25,11}},"PM2.5",28.4,Monitor9),

  Monitor11=pollution:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",600,Monitor10),
  Monitor12=pollution:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",300,Monitor11),

  Monitor12.

getOverLimit_Correct(Monitor) ->
  [
    ?_assertEqual(6,pollution:getOverLimit(Monitor)),
    ?_assertEqual(0,pollution:getOverLimit(pollution:createMonitor()))
  ].

