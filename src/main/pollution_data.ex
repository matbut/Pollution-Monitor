defmodule PollutionData do
  @moduledoc false

  #defstruct datetime: {{0,0,0},{0,0,0}}, location: {0,0}, pollutionLevel: 0

  def measureTime(moduleName,funName,args) do
  {time,val} = fn -> apply(moduleName, funName,args) end |> :timer.tc
  {time/1000000,val}
  end

  def importData() do

    :pollution_gen_server_sup.start_link()

    pollution_data=importLinesFromCSV()
    stations=identifyStations(pollution_data)

    time1 = measureTime(PollutionData,:loadStations,[stations])
    time2 = measureTime(PollutionData,:loadValues,[pollution_data])
    {time1,time2}

    #:pollution_gen_server.stop()
  end


  def importLinesFromCSV() do
    File.read!("../../data/pollution.csv") |> String.split() |> Enum.map(fn line ->  convert(line) end)
  end

  def convert(line) do
    [date, time, cord_x, cord_y,val] = String.split(line,",")

    date = date |> String.split("-") |> Enum.reverse() |> Enum.map(fn x -> elem(Integer.parse(x),0) end) |> :erlang.list_to_tuple()
    time = time<>":0" |> String.split(":") |> Enum.map(fn x -> elem(Integer.parse(x),0)end) |> :erlang.list_to_tuple()
    cords = {elem(Float.parse(cord_x),0),elem(Float.parse(cord_y),0)}
    val = elem(Float.parse(val),0)
    %{datetime: {date,time}, location: cords, pollutionLevel: val}
  end

  def loadStationsOld(pollution_data) do

    Enum.each(pollution_data,fn record ->
      :pollution_gen_server.addStation(Enum.join(Tuple.to_list(record.location)),record.location) end)
  end

  def loadStations(stations) do
    Enum.each(stations,fn {name,cords} ->
      :pollution_gen_server.addStation(name,cords) end)
  end

  def loadValues(pollution_data) do
    Enum.each(pollution_data,fn record ->
      :pollution_gen_server.addValue(record.location,record.datetime,"PM10",record.pollutionLevel) end)
  end

  def identifyStations(pollution_data) do
    stations = pollution_data |> Enum.reduce(%{},fn record,map ->
      Map.put(map,stationName(record.location),record.location) end)
    stations
  end

  def stationName({x_cord,y_cord}) do
    "station_#{x_cord}_#{y_cord}"
  end

end
