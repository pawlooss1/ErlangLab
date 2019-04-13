%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2019 09:48
%%%-------------------------------------------------------------------
-module(pingpong).
-author("pawel").

%% API
-export([start/0, stop/0, play/1, ping_loop/1, pong_loop/1]).

start() ->
  register(ping, spawn(pingpong, ping_loop, [0])),
  register(pong, spawn(pingpong, pong_loop, [0])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! N.

ping_loop(Sum) ->
  receive
    stop -> ok;
    0 -> ping_loop(Sum);
    N -> io:format("Ping! ~w Suma: ~w~n", [N, Sum+N]),
      timer:sleep(300),
      pong ! (N-1),
      ping_loop(Sum+N)
  after
    10000 -> ok
  end.

pong_loop(Sum) ->
  receive
    stop -> ok;
    0 -> pong_loop(Sum);
    N -> io:format("Pong! ~w Suma: ~w~n", [N, Sum+N]),
      timer:sleep(300),
      ping ! (N-1),
      pong_loop(Sum+N)
  after
    10000 -> ok
  end.