%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2019 14:42
%%%-------------------------------------------------------------------
-module(onp).
-author("pawel").

%% API
-export([onp/1]).

onp(Expression) ->
  ParsedInput = string:tokens(Expression, " "),
  calculate(ParsedInput, []).


calculate([], Stack) ->
  hd(Stack);

calculate(["+" | T], [B,A |Stack]) ->
  calculate(T, [A+B | Stack]);

calculate(["-" | T], [B,A |Stack]) ->
  calculate(T, [A-B | Stack]);

calculate(["*" | T], [B,A |Stack]) ->
  calculate(T, [A*B | Stack]);

calculate(["/" | T], [B,A |Stack]) ->
  calculate(T, [A div B | Stack]);

calculate(["sqrt" | T], [A, Stack]) ->
  calculate(T, [math:sqrt(A) | Stack]);

calculate(["pow" | T], [B,A | Stack]) ->
  calculate(T, [math:pow(A, B) | Stack]);

calculate(["sin" | T], [A | Stack]) ->
  calculate(T, [math:sin(A) | Stack]);

calculate(["cos" | T], [A |Stack]) ->
  calculate(T, [math:cos(A) | Stack]);

calculate(["tan" | T], [A | Stack]) ->
  calculate(T, [math:tan(A) | Stack]);

calculate([H | T], Stack) ->
  Number = list_to_float(H),
  calculate(T, [Number | Stack]).