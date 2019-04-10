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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4]).

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
  Pred = fun
           (#measurement{type = Type, datetime = Date}) -> DateTime /= Date or MeasureType /= Type
         end,
  UpdatedMeasurements = lists:filter(Pred, Measurements),
  Monitor#{Key := Station#station{measurements = UpdatedMeasurements}}.

removeValue(Coordinates, DateTime, MeasureType, Monitor) when is_tuple(Coordinates) ->
  Key = getKey(Coordinates, coordinates, Monitor),
  removeValueSecure(Key, DateTime, MeasureType, Monitor);
removeValue(Name, DateTime, MeasureType, Monitor) when is_list(Name) ->
  Key = getKey(Name, name, Monitor),
  removeValueSecure(Key, DateTime, MeasureType, Monitor).