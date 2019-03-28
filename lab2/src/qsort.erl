%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2019 09:59
%%%-------------------------------------------------------------------
-module(qsort).
-author("pawel").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) ->
  [rand:uniform(Max - Min + 1) + Min - 1 || A <- lists:seq(1,N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  {Time1, Time2}.