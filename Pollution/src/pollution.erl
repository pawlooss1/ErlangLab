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

% Monitor : {#{Name => Station}, #{Coordinates => Name}}
createMonitor() -> {#{}, #{}}.

addStation(Monitor, Name, Coordinates) ->
  {NameStation, CoordsName} = Monitor,
  ContainsName = maps:is_key(Name, NameStation),
  ContainsCoords = maps:is_key(Coordinates, CoordsName),
  addStationSecure(Name, Coordinates, Monitor, ContainsName, ContainsCoords).

addValue(Monitor, Key, DateTime, MeasureType, Value) ->
  applySecureFunction(Key, DateTime, MeasureType, Value, Monitor, fun addValueSecure/6).

removeValue(Monitor, Key, DateTime, MeasureType) ->
  applySecureFunction(Key, DateTime, MeasureType, none, Monitor, fun removeValueSecure/6).

getOneValue(Monitor, Key, DateTime, MeasureType) ->
  applySecureFunction(Key, DateTime, MeasureType, none, Monitor, fun getValueSecure/6).

getStationMean(Monitor, Key, MeasureType)->
  applySecureFunction(Key, none, MeasureType, none, Monitor, fun getStationMeanSecure/6).

getDailyMean(Monitor, Date, MeasureType) ->
  applySecureFunction(none, Date, MeasureType, none, Monitor, fun getDailyMeanSecure/5).

getHourlyMean(Monitor, Key, Time, MeasureType) ->
  applySecureFunction(Key, Time, MeasureType, 0, Monitor, fun hourlyMeanSecure/6).

%% private functions

addStationSecure(Name, Coordinates, Monitor, false, false) ->
  {NameStation, CoordsName} = Monitor,
  Station = #station{coordinates = Coordinates, name = Name, measurements = []},
  {NameStation#{Name => Station}, CoordsName#{Coordinates => Name}};
addStationSecure(_, _, Monitor, _, _) ->
  stationExists.

applySecureFunction(none, DateTime, MeasureType, Value, Monitor, Function) ->
  {NameStation, CoordsName} = Monitor,
  Function(DateTime, MeasureType, Value, NameStation, CoordsName);
applySecureFunction(Coords, DateTime, MeasureType, Value, Monitor, Function) when is_tuple(Coords) ->
  {NameStation, CoordsName} = Monitor,
  GoodKey = maps:is_key(Coords, CoordsName),
  if
    GoodKey -> Function(maps:get(Coords, CoordsName), DateTime, MeasureType, Value, NameStation, CoordsName);
    true -> noSuchStation
  end;
applySecureFunction(Name, DateTime, MeasureType, Value, Monitor, Function) when is_list(Name) ->
  {NameStation, CoordsName} = Monitor,
  GoodKey = maps:is_key(Name, NameStation),
  if
    GoodKey -> Function(Name, DateTime, MeasureType, Value, NameStation, CoordsName);
    true -> noSuchStation
  end.

unpackStationAndMeasurements(Name, NameStation) ->
  Station = maps:get(Name, NameStation),
  #station{measurements = Measurements} = Station,
  {Station, Measurements}.

safeUpdate(Measurements, Measurement, false) ->
  [Measurement | Measurements];
safeUpdate(Measurements, _, _) ->
  Measurements.

addValueSecure(Name, DateTime, MeasureType, Value, NameStation, CoordsName) ->
  {Station, Measurements} = unpackStationAndMeasurements(Name, NameStation),
  Measurement = #measurement{type = MeasureType, value = Value, datetime = DateTime},
  F = fun({measurement, Type, _, Date}, Acc) -> Acc or ((DateTime == Date) and (MeasureType == Type)) end,
  CheckList = lists:foldl(F, false, Measurements),
  UpdatedMeasurements = safeUpdate(Measurements, Measurement, CheckList),
  {NameStation#{Name := Station#station{measurements = UpdatedMeasurements}}, CoordsName}.

removeValueSecure(Name, DateTime, MeasureType, _, NameStation, CoordsName) ->
  {Station, Measurements} = unpackStationAndMeasurements(Name, NameStation),
  Pred = fun({measurement, Type, _, Date}) -> ((DateTime /= Date) or (MeasureType /= Type)) end,
  UpdatedMeasurements = lists:filter(Pred, Measurements),
  {NameStation#{Name := Station#station{measurements = UpdatedMeasurements}}, CoordsName}.

unpackValue([]) -> noMeasurement;
unpackValue([#measurement{value = Value}|_]) -> Value.

getValueSecure(Name, DateTime, MeasureType, _, NameStation, _) ->
  {_, Measurements} = unpackStationAndMeasurements(Name, NameStation),
  Pred = fun({measurement, Type, _, Date}) -> ((DateTime == Date) and (MeasureType == Type)) end,
  ExtractedMeasurement = lists:filter(Pred, Measurements),
  unpackValue(ExtractedMeasurement).

getStationMeanSecure(Name, _, MeasureType, _, NameStation, _) ->
  {_, Measurements} = unpackStationAndMeasurements(Name, NameStation),
  TypeMeasurements = lists:filter(fun(#measurement{type = Type}) -> Type == MeasureType end, Measurements),
  calculateMean(TypeMeasurements).

mergeAllMeasurements(NameStation) ->
  Stations = maps:values(NameStation),
  Concatenator = fun(#station{measurements = Measurements}, List) -> List ++ Measurements end,
  lists:foldl(Concatenator, [], Stations).

getDailyMeanSecure(Date, MeasureType, _, NameStation, _) ->
  AllMeasurements = mergeAllMeasurements(NameStation),
  Pred = fun(#measurement{type = Type, datetime = {D, _}}) -> ((Type == MeasureType) and (D == Date)) end,
  Measurements = lists:filter(Pred, AllMeasurements),
  calculateMean(Measurements).

hourlyMeanSecure(Name, Time, MeasureType, _, NameStation, _) ->
  {_, Measurements} = unpackStationAndMeasurements(Name, NameStation),
  Pred = fun(#measurement{type = Type, datetime = {_, T}}) -> ((Type == MeasureType) and (T == Time)) end,
  FilteredMeasurements = lists:filter(Pred, Measurements),
  calculateMean(FilteredMeasurements).

calculateMean([]) -> noMeasurements;
calculateMean(Measurements) ->
  FSum = fun(#measurement{value = Val}, Acc) -> Acc + Val end,
  FCount = fun(_, Acc) -> Acc + 1 end,
  Sum = lists:foldl(FSum, 0, Measurements),
  Count = lists:foldl(FCount, 0, Measurements),
  Sum / Count.
