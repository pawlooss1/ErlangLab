%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2019 14:21
%%%-------------------------------------------------------------------
-module(pollution).
-author("pawel").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/4]).

-record(station, {coordinates, name, measurements}).
-record(measurement, {type, value, datetime}).

createMonitor() -> #{}.

addStationSecure(Name, Coordinates, Monitor, false, false) ->
  maps:put({Name, Coordinates}, #station{coordinates = Coordinates, name = Name, measurements = []}, Monitor);
addStationSecure(_, _, Monitor, _, _) ->
  Monitor.

checkName(GivenName) ->
  fun
    ({Name, _}, A) -> (Name == GivenName) or A
  end.
checkCoords(GivenCoordinates) ->
  fun
    ({_, Coordinates}, A) -> (Coordinates == GivenCoordinates) or A
  end.

addStation(Name, Coordinates, Monitor) ->
  Keys = maps:keys(Monitor),
  ContainsName = lists:foldl(checkName(Name), false, Keys),
  ContainsCoords = lists:foldl(checkCoords(Coordinates), false, Keys),
  addStationSecure(Name, Coordinates, Monitor, ContainsName, ContainsCoords).

getKey(Value, coordinates, Monitor) ->
  Keys = maps:keys(Monitor),
  lists:filter(fun ({_, Coords}) -> Coords == Value end, Keys);
getKey(Value, name, Monitor) ->
  Keys = maps:keys(Monitor),
  lists:filter(fun ({Name, _}) -> Name == Value end, Keys).

addValueSecure([], _, Monitor) ->
  Monitor;
addValueSecure([Key|_], Measurement, Monitor) ->
  #{Key := Station} = Monitor,
  #station{measurements = Measurements} = Station,
  UpdatedMeasurements = (Measurements -- [Measurement]) ++ [Measurement],
  Monitor#{Key := Station#station{measurements = UpdatedMeasurements}}.

addValue(Coordinates, DateTime, MeasureType, Value, Monitor) when is_tuple(Coordinates) ->
  Measurement = #measurement{type = MeasureType, value = Value, datetime = DateTime},
  Key = getKey(Coordinates, coordinates, Monitor),
  addValueSecure(Key, Measurement, Monitor);
addValue(Name, DateTime, MeasureType, Value, Monitor) when is_list(Name) ->
  Measurement = #measurement{type = MeasureType, value = Value, datetime = DateTime},
  Key = getKey(Name, name, Monitor),
  addValueSecure(Key, Measurement, Monitor).

removeValueSecure([], _, _, Monitor) ->
  Monitor;
removeValueSecure([Key|_], DateTime, MeasureType, Monitor) ->
  #{Key := Station} = Monitor,
  #station{measurements = Measurements} = Station,
  Pred = fun ({measurement, Type, _, Date}) -> ((DateTime /= Date) or (MeasureType /= Type)) end,
  UpdatedMeasurements = lists:filter(Pred, Measurements),
  Monitor#{Key := Station#station{measurements = UpdatedMeasurements}}.

removeValue(Coordinates, DateTime, MeasureType, Monitor) when is_tuple(Coordinates) ->
  Key = getKey(Coordinates, coordinates, Monitor),
  removeValueSecure(Key, DateTime, MeasureType, Monitor);
removeValue(Name, DateTime, MeasureType, Monitor) when is_list(Name) ->
  Key = getKey(Name, name, Monitor),
  removeValueSecure(Key, DateTime, MeasureType, Monitor).

getValueSecure([], _, _, _) ->
  "No measurement";
getValueSecure([Key|_], DateTime, MeasureType, Monitor) ->
  #{Key := Station} = Monitor,
  #station{measurements = Measurements} = Station,
  Pred = fun ({measurement, Type, _, Date}) -> ((DateTime == Date) and (MeasureType == Type)) end,
  [ExtractedMeasurement] = lists:filter(Pred, Measurements),
  #measurement{value = Value} = ExtractedMeasurement,
  Value.

getOneValue(Coordinates, DateTime, MeasureType, Monitor) when is_tuple(Coordinates) ->
  Key = getKey(Coordinates, coordinates, Monitor),
  getValueSecure(Key, DateTime, MeasureType, Monitor);
getOneValue(Name, DateTime, MeasureType, Monitor) when is_list(Name) ->
  Key = getKey(Name, name, Monitor),
  getValueSecure(Key, DateTime, MeasureType, Monitor).

getMeanSecure(Key, MeasureType, Monitor) ->
  #{Key := Station} = Monitor,
  #station{measurements = Measurements} = Station,
  TypeMeasurements = lists:filter(fun (#measurement{type = Type}) -> Type == MeasureType end, Measurements),
  calculateMean(TypeMeasurements).

getStationMean(Coordinates, MeasureType, Monitor) when is_tuple(Coordinates) ->
  Key = getKey(Coordinates, coordinates, Monitor),
  getMeanSecure(Key, MeasureType, Monitor);
getStationMean(Name, MeasureType, Monitor) when is_list(Name) ->
  Key = getKey(Name, name, Monitor),
  getMeanSecure(Key, MeasureType, Monitor).

getDailyMean(Date, MeasureType, Monitor) ->
  Stations = maps:values(Monitor),
  Concatenator = fun (#station{measurements = Measurements}, List) -> List ++ Measurements end,
  AllMeasurements = lists:foldl(Concatenator, [], Stations),
  Measurements = lists:filter(fun(#measurement{type = Type, datetime = {D, _}}) -> ((Type == MeasureType) and (D == Date)) end, AllMeasurements),
  calculateMean(Measurements).

hourlyMeanSecure(Key, MeasureType, Time, Monitor) ->
  #{Key := Station} = Monitor,
  #station{measurements = Measurements} = Station,
  FilteredMeasurements = lists:filter(fun(#measurement{type = Type, datetime = {_, T}}) -> ((Type == MeasureType) and (T == Time)) end, Measurements),
  calculateMean(FilteredMeasurements).

getHourlyMean(Coordinates, MeasureType, Time, Monitor) when is_tuple(Coordinates) ->
  Key = getKey(Coordinates, coordinates, Monitor),
  hourlyMeanSecure(Key, MeasureType, Time, Monitor);
getHourlyMean(Name, MeasureType, Time, Monitor) when is_list(Name) ->
  Key = getKey(Name, name, Monitor),
  hourlyMeanSecure(Key, MeasureType, Time, Monitor).

calculateMean(Measurements) ->
  FSum = fun (#measurement{value = Val}, Acc) -> Acc + Val end,
  FCount = fun (_, Acc) -> Acc + 1 end,
  Sum = lists:foldl(FSum, 0, Measurements),
  Count = lists:foldl(FCount, 0, Measurements),
  Sum / Count.
