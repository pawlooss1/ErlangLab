%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2019 17:26
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("pawel").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/3]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

stop() ->
  pollutionServer ! stop.

init() ->
  loop(pollution:createMonitor()).

%% server

checkAndLoop(ClientPid, OldMonitor, noSuchStation) ->
  ClientPid ! {reply, noSuchStation},
  loop(OldMonitor);
checkAndLoop(ClientPid, OldMonitor, Result) when is_number(Result) ->
  ClientPid ! {reply, Result},
  loop(OldMonitor);
checkAndLoop(ClientPid, _, NewMonitor) ->
  ClientPid ! {reply, ok},
  loop(NewMonitor).

loop(Monitor) ->
  receive
    stop -> ok;
    {request, Pid, addStation, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, addStation, [Monitor | Arguments]));
    {request, Pid, addValue, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, addValue, [Monitor | Arguments]));
    {request, Pid, removeValue, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, removeValue, [Monitor | Arguments]));
    {request, Pid, getOneValue, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, getOneValue, [Monitor | Arguments]));
    {request, Pid, getStationMean, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, getStationMean, [Monitor | Arguments]));
    {request, Pid, getDailyMean, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, getDailyMean, [Monitor | Arguments]));
    {request, Pid, getHourlyMean, Arguments} ->
      checkAndLoop(Pid, Monitor, apply(pollution, getHourlyMean, [Monitor | Arguments]))
  end.

%% client

call(Message, Arguments) ->
  pollution_server ! {request, self(), Message, Arguments},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, Coordinates) -> call(addStation, [Name, Coordinates]).
addValue(Key, DateTime, MeasureType, Value) -> call(addValue, [Key, DateTime, MeasureType, Value]).
removeValue(Key, DateTime, MeasureType) -> call(removeValue, [Key, DateTime, MeasureType]).
getOneValue(Key, DateTime, MeasureType) -> call(getOneValue, [Key, DateTime, MeasureType]).
getStationMean(Key, MeasureType) -> call(getStationMean, [Key, MeasureType]).
getDailyMean(Date, MeasureType) -> call(getDailyMean, [Date, MeasureType]).
getHourlyMean(Key, Time, MeasureType) -> call(getHourlyMean, [Key, Time, MeasureType]).