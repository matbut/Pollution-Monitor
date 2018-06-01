defmodule PollutionDataStream do
  @moduledoc false

  def importData() do

    :pollution_gen_server_sup.start_link()

    pollutionDataStream=importLinesFromCSV()
    stationsStream=identifyStations(pollutionDataStream)

    time1 = PollutionData.measureTime(PollutionDataStream,:loadStations,[stationsStream])
    time2 = PollutionData.measureTime(PollutionDataStream,:loadValues,[pollutionDataStream])
    :pollution_gen_server.stop()
    {time1,time2}

  end

  def importLinesFromCSV() do
    File.stream!("../../data/pollution.csv") |> Stream.map(fn line ->  PollutionData.convert(line) end) |> Enum.to_list()
  end

  def loadStations(stationsStream) do
    stationsStream
    |> Stream.map(fn station ->
      :pollution_gen_server.addStation(station.name,station.location) end)
    |> Enum.reduce_while(:ok,fn ret,acc -> if ret==:ok do {:cont, acc} else {:halt,{:error,{"load station error",ret}}} end end)
  end

  def loadValues(pollutionDataStram) do
    pollutionDataStram
    |> Stream.map(fn record ->
    :pollution_gen_server.addValue(record.location,record.datetime,"PM10",record.pollutionLevel) end)
    |> Enum.reduce({{:inserted,0},{:all,0}},fn ret,{{:inserted,i},{:all,a}} -> if ret==:ok do {{:inserted,i+1},{:all,a+1}} else {{:inserted,i},{:all,a+1}} end end)
  end

  def identifyStations(pollutionDataStream) do
    pollutionDataStream
    |> Stream.map(fn record -> %{name: PollutionData.stationName(record.location), location: record.location} end)
    |> Stream.uniq()
  end
end
