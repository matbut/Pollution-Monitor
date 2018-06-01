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

    :pollution_gen_server.stop()
    {time1,time2}
  end

  def importLinesFromCSV() do
    File.read!("../../data/pollution.csv") |> String.split() |> Enum.map(fn line ->  convert(line) end)
  end

  def convert(line) do
    [date, time, cord_x, cord_y,val] = String.split(line,",")

    date = date |> String.split("-") |> Enum.reverse() |> Enum.map(fn x -> elem(Integer.parse(x),0) end) |> :erlang.list_to_tuple()
    time = time<>":0" |> String.split(":") |> Enum.map(fn x -> elem(Integer.parse(x),0)end) |> :erlang.list_to_tuple()
    cords = {elem(Float.parse(cord_x),0),elem(Float.parse(cord_y),0)}
    val = elem(Integer.parse(val),0)
    %{datetime: {date,time}, location: cords, pollutionLevel: val}
  end

  def loadStationsOld(pollution_data) do

    Enum.each(pollution_data,fn record ->
      :pollution_gen_server.addStation(Enum.join(Tuple.to_list(record.location)),record.location) end)
  end

  def loadStations(stations) do
    stations
    |> Enum.map(fn {name,cords}->:pollution_gen_server.addStation(name,cords) end)
    |> Enum.reduce_while(:ok,fn ret,acc -> if ret==:ok do {:cont, acc} else {:halt,{:error,{"load station error",ret}}} end end)
  end

  def loadValues(pollutionData) do
    pollutionData
    |> Enum.map(fn record ->
      #{record,:pollution_gen_server.addValue(record.location,record.datetime,"PM10",record.pollutionLevel)} end)
      #|> Enum.reduce_while(:ok,fn {record,ret},acc -> if ret==:ok do {:cont, acc} else {:halt,{:error,{"load values error",ret,record}}} end end)
      :pollution_gen_server.addValue(record.location,record.datetime,"PM10",record.pollutionLevel) end)
    |> Enum.reduce({{:inserted,0},{:all,0}},fn ret,{{:inserted,i},{:all,a}} -> if ret==:ok do {{:inserted,i+1},{:all,a+1}} else {{:inserted,i},{:all,a+1}} end end)
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
