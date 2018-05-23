%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2018 00:00
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("mateusz").
-compile([export_all, debug_info]).
-include_lib("eunit/include/eunit.hrl").

addStation_test_() ->
  {setup,
    fun addStationStart/0,
    fun endTest/1,
    fun (_) ->
      [addStation_SameCords(),
        addStation_SameName(),
        addStations_IncorrectName(),
        addStations_IncorrectCords()
     ]
    end}.

addStationStart() ->
  ok=pollution_server:start(),
  ok=pollution_server:addStation("Krasickiego",{13.23,17.25}),
  ok=pollution_server:addStation("Bulwarowa",{120,150}),
  nothing.

endTest(_) ->
  ok=pollution_server:stop().

addStation_SameCords() ->
  [
    ?_assertMatch({error,_},pollution_server:addStation("Slowackiego",{13.23,17.25})),
    ?_assertMatch({error,_},pollution_server:addStation("Slowackiego",{120,150}))
  ].

addStation_SameName() ->
  %[?_assertError(ADD_STATION_ERROR_MES,pollution_server:addStation("Krasickiego",{21.16,17.25}))].
  [
    ?_assertMatch({error,_},pollution_server:addStation("Krasickiego",{21.16,17.25})),
    ?_assertMatch({error,_},pollution_server:addStation("Bulwarowa",{17,150}))
  ].

addStations_IncorrectName() ->
  [
    ?_assertMatch({error,_},pollution_server:addStation(bujaka,{133.5,31.4})),
    ?_assertMatch({error,_},pollution_server:addStation([15,65,66],{133.5,31.4})),
    ?_assertMatch({error,_},pollution_server:addStation(["Stacja","Bujaka"],{133.5,31.4})),
    ?_assertMatch({error,_},pollution_server:addStation({"Stacja","Bujaka"},{133.5,31.4}))
  ].

addStations_IncorrectCords() ->
  [
    ?_assertMatch({error,_},pollution_server:addStation("Bujaka",{five,21})),
    ?_assertMatch({error,_},pollution_server:addStation("Bujaka",{13.4,[14,153]})),
    ?_assertMatch({error,_},pollution_server:addStation("Bujaka",{13.4,{14,153}}))
  ].


addGetValue_test_() ->
  {setup,
    fun addGetValueStart/0,
    fun endTest/1,
    fun (_) ->
      [
        addGetValues_Correct(),
        addValues_SameKey(),
        addValues_IncorrectStation()
      ]
    end}.

addGetValueStart() ->
  ok=pollution_server:start(),
  ok=pollution_server:addStation("Krasickiego",{13.23,17.25}),
  ok=pollution_server:addStation("Bulwarowa",{120,150}),

  ok=pollution_server:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",79),
  ok=pollution_server:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",116),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",17),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",215).

addGetValues_Correct() ->
  [
    ?_assertEqual({ok,79},pollution_server:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertEqual({ok,79},pollution_server:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertEqual({ok,116},pollution_server:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM2.5")),
    ?_assertEqual({ok,116},pollution_server:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5")),
    ?_assertEqual({ok,17},pollution_server:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertEqual({ok,17},pollution_server:getOneValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertEqual({ok,215},pollution_server:getOneValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5")),
    ?_assertEqual({ok,215},pollution_server:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5"))
  ].

addValues_SameKey() ->
  [
    ?_assertMatch({error,_},pollution_server:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",195)),
    ?_assertMatch({error,_},pollution_server:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",7)),
    ?_assertMatch({error,_},pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",17)),
    ?_assertMatch({error,_},pollution_server:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",215))
  ].

addValues_IncorrectStation() ->
  [
    ?_assertMatch({error,_},pollution_server:addValue("Bujaka",{{1997,11,01},{11,15,27}},"PM10",195)),
    ?_assertMatch({error,_},pollution_server:addValue(bujaka,{{1997,11,01},{11,15,27}},"PM2.5",7)),
    ?_assertMatch({error,_},pollution_server:addValue(["Stacja","Bujaka"],{{1997,11,01},{11,15,27}},"PM10",17)),
    ?_assertMatch({error,_},pollution_server:addValue({"Stacja","Bujaka"},{{1997,11,01},{12,15,27}},"PM2.5",215)),

    ?_assertMatch({error,_},pollution_server:addValue({5,21},{{1997,11,01},{12,15,27}},"PM2.5",215)),
    ?_assertMatch({error,_},pollution_server:addValue({five,21},{{1997,11,01},{12,15,27}},"PM2.5",215)),
    ?_assertMatch({error,_},pollution_server:addValue({13.4,[14,153]},{{1997,11,01},{12,15,27}},"PM2.5",215)),
    ?_assertMatch({error,_},pollution_server:addValue({13.4,{14,153}},{{1997,11,01},{12,15,27}},"PM2.5",215))
  ].

removeValue_test_() ->
  {setup,
    fun addGetValueStart/0,
    fun endTest/1,
    fun (_) ->
      [
        removeValue_Correct(),
        removeValue_Double(),
        removeValue_IncorrectKey()
      ]
    end}.

removeValue_Correct() ->
  ok=pollution_server:removeValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10"),
  ok=pollution_server:removeValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5"),
  ok=pollution_server:removeValue({120,150},{{1997,11,01},{11,15,27}},"PM10"),
  ok=pollution_server:removeValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5"),
  [
    ?_assertMatch({error,_},pollution_server:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5"))
  ].

removeValue_Double() ->
  ok=pollution_server:removeValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10"),
  ok=pollution_server:removeValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5"),
  ok=pollution_server:removeValue({120,150},{{1997,11,01},{11,15,27}},"PM10"),
  ok=pollution_server:removeValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5"),
  [
    ?_assertMatch({error,_},pollution_server:getOneValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue({120,150},{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:getOneValue("Bulwarowa",{{1997,11,01},{12,15,27}},"PM2.5"))
  ].

removeValue_IncorrectKey() ->
  [
    ?_assertMatch({error,_},pollution_server:removeValue("Bujaka",{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:removeValue(bujaka,{{1997,11,01},{11,15,27}},"PM2.5")),
    ?_assertMatch({error,_},pollution_server:removeValue(["Stacja","Bujaka"],{{1997,11,01},{11,15,27}},"PM10")),
    ?_assertMatch({error,_},pollution_server:removeValue({"Stacja","Bujaka"},{{1997,11,01},{12,15,27}},"PM2.5")),

    ?_assertMatch({error,_},pollution_server:removeValue({5,21},{{1997,11,01},{12,15,27}},"PM2.5")),
    ?_assertMatch({error,_},pollution_server:removeValue({five,21},{{1997,11,01},{12,15,27}},"PM2.5")),
    ?_assertMatch({error,_},pollution_server:removeValue({13.4,[14,153]},{{1997,11,01},{12,15,27}},"PM2.5")),
    ?_assertMatch({error,_},pollution_server:removeValue({13.4,{14,153}},{{1997,11,01},{12,15,27}},"PM2.5"))
  ].

getStationMean_test_() ->
  {setup,
    fun getStationMeanStart/0,
    fun endTest/1,
    fun (_) ->
      [
        getStationMean_Correct(),
        getStationMean_IncorrectStation()
      ]
    end}.

getStationMeanStart() ->
  ok=pollution_server:start(),
  ok=pollution_server:addStation("Krasickiego",{13.23,17.25}),
  ok=pollution_server:addStation("Bulwarowa",{120,150}),

  ok=pollution_server:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",2000),
  ok=pollution_server:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",3000),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",10),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",100),
  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{15,05,46}},"PM10",20),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{2,25,11}},"PM2.5",200),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",30),
  ok=pollution_server:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",300).

getStationMean_Correct() ->
  [
    ?_assertEqual({ok,2000.0},pollution_server:getStationMean("Krasickiego","PM10")),
    ?_assertEqual({ok,2000.0},pollution_server:getStationMean({13.23,17.25},"PM10")),
    ?_assertEqual({ok,3000.0},pollution_server:getStationMean("Krasickiego","PM2.5")),
    ?_assertEqual({ok,3000.0},pollution_server:getStationMean({13.23,17.25},"PM2.5")),

    ?_assertEqual({ok,20.0},pollution_server:getStationMean("Bulwarowa","PM10")),
    ?_assertEqual({ok,20.0},pollution_server:getStationMean({120,150},"PM10")),
    ?_assertEqual({ok,200.0},pollution_server:getStationMean("Bulwarowa","PM2.5")),
    ?_assertEqual({ok,200.0},pollution_server:getStationMean({120,150},"PM2.5"))
  ].

getStationMean_IncorrectStation() ->
  [
    ?_assertMatch({error,_},pollution_server:getStationMean("Bujaka","PM10")),
    ?_assertMatch({error,_},pollution_server:getStationMean({12.3,43.4},"PM10")),
    ?_assertMatch({error,_},pollution_server:getStationMean("Bulwarowa","NO2"))
  ].

getDailyMean_test_() ->
  {setup,
    fun getDailyMeanStart/0,
    fun endTest/1,
    fun (_) ->
      [
        getDailyMean_Correct(),
        getDailyMean_IncorrectStation()
      ]
    end}.

getDailyMeanStart() ->
  ok=pollution_server:start(),
  ok=pollution_server:addStation("Krasickiego",{13.23,17.25}),
  ok=pollution_server:addStation("Bulwarowa",{120,150}),

  ok=pollution_server:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",30),
  ok=pollution_server:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",300),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",10),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{12,15,27}},"PM2.5",100),
  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{15,05,46}},"PM10",20),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{2,25,11}},"PM2.5",200),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",6000),
  ok=pollution_server:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",3000).
getDailyMean_Correct() ->
  [
    ?_assertEqual({ok,20.0},pollution_server:getDailyMean({1997,11,01},"PM10")),
    ?_assertEqual({ok,200.0},pollution_server:getDailyMean({1997,11,01},"PM2.5")),
    ?_assertEqual({ok,6000.0},pollution_server:getDailyMean({1997,11,02},"PM10")),
    ?_assertEqual({ok,3000.0},pollution_server:getDailyMean({1998,11,01},"PM2.5"))
  ].

getDailyMean_IncorrectStation() ->
  [
    ?_assertMatch({error,_},pollution_server:getStationMean("Bujaka","PM10")),
    ?_assertMatch({error,_},pollution_server:getStationMean({12.3,43.4},"PM10")),
    ?_assertMatch({error,_},pollution_server:getStationMean("Bulwarowa","NO2"))
  ].

getOverLimit_test_() ->
  {setup,
    fun getOverLimitStart/0,
    fun endTest/1,
    fun (_) ->
      [
        getOverLimit_Correct()
      ]
    end}.

getOverLimitStart()->
  ok=pollution_server:start(),
  ok=pollution_server:addStation("Krasickiego",{13.23,17.25}),
  ok=pollution_server:addStation("Bulwarowa",{120,150}),

  ok=pollution_server:addValue("Krasickiego",{{1997,11,01},{11,15,27}},"PM10",51),
  ok=pollution_server:addValue({13.23,17.25},{{1997,11,01},{11,15,27}},"PM2.5",31),

  ok=pollution_server:addValue("Krasickiego",{{1997,11,02},{11,15,27}},"PM10",50),
  ok=pollution_server:addValue({13.23,17.25},{{1997,11,02},{11,15,27}},"PM2.5",30),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,15,27}},"PM10",50.3),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{11,15,27}},"PM2.5",30.2),
  ok=pollution_server:addValue("Bulwarowa",{{1997,11,01},{11,05,46}},"PM10",49.7),
  ok=pollution_server:addValue({120,150},{{1997,11,01},{11,25,11}},"PM2.5",28.4),

  ok=pollution_server:addValue("Bulwarowa",{{1997,11,02},{11,15,27}},"PM10",600),
  ok=pollution_server:addValue({120,150},{{1998,11,01},{12,15,27}},"PM2.5",300).

getOverLimit_Correct() ->
  [
    ?_assertEqual({ok,4},pollution_server:getOverLimit({1997,11,01},11)),
    ?_assertEqual({ok,0},pollution_server:getOverLimit({1997,11,03},11)),
    ?_assertEqual({ok,0},pollution_server:getOverLimit({1997,11,02},12)),
    ?_assertEqual({ok,1},pollution_server:getOverLimit({1998,11,01},12))
  ].
