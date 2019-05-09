%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 10:33
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("pawel").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3]).
-export([init/1, handle_call/3]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,0,[]).

stop() ->
  gen_server:call(pollution_gen_server,terminate).

addStation(Name, Coordinates) ->
  gen_server:call(pollution_gen_server, [addStation, Name, Coordinates]).

addValue(Key, DateTime, MeasureType, Value) ->
  gen_server:call(pollution_gen_server, [addValue, Key, DateTime, MeasureType, Value]).

getOneValue(Key, DateTime, MeasureType) ->
  gen_server:call(pollution_gen_server, [getOneValue, Key, DateTime, MeasureType]).




%%%%%%    SPRAWDZ BIF unlink



checkAndLoop(OldMonitor, Code) when is_atom(Code)->
  {reply, Code, OldMonitor};
checkAndLoop(OldMonitor, Result) when is_number(Result) ->
  {reply, Result, OldMonitor};
checkAndLoop(_, NewMonitor) ->
  {reply, ok, NewMonitor}.

init(_) -> {ok, pollution:createMonitor()}.
handle_call(terminate, _From, N) -> {stop, normal, ok, N};
handle_call([FunName|Arguments], _From, Monitor) ->
  checkAndLoop(Monitor, apply(pollution, FunName, [Monitor | Arguments])).