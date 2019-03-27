%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2019 14:11
%%%-------------------------------------------------------------------
-module(myLists).
-author("pawel").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloats/2]).

contains([], _) ->
  false;
contains([H | T], Value) ->
  H == Value orelse contains(T, Value).

duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).

sumFloats([]) ->
  0;
sumFloats([H | T]) when is_float(H) ->
  H + sumFloats(T);
sumFloats([_ | T]) ->
  sumFloats(T).

sumFloats([], Sum) ->
  Sum;
sumFloats([H | T], Sum) when is_float(H) ->
  sumFloats(T, Sum + H);
sumFloats([_ | T], Sum) ->
  sumFloats(T, Sum).