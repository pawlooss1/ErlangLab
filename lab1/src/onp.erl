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

calculate([H | T], Stack) when H == "+" ->
  [B | Stack1] = Stack,
  [A | Stack2] = Stack1,
  calculate(T, [A+B | Stack2]);

calculate([H | T], Stack) when H == "-" ->
  [B | Stack1] = Stack,
  [A | Stack2] = Stack1,
  calculate(T, [A-B | Stack2]);

calculate([H | T], Stack) when H == "*" ->
  [B | Stack1] = Stack,
  [A | Stack2] = Stack1,
  calculate(T, [A*B | Stack2]);

calculate([H | T], Stack) when H == "/" ->
  [B | Stack1] = Stack,
  [A | Stack2] = Stack1,
  calculate(T, [A div B | Stack2]);

calculate([H | T], Stack) when H == "sqrt" ->
  [A | Stack1] = Stack,
  calculate(T, [math:sqrt(A) | Stack1]);

calculate([H | T], Stack) when H == "pow" ->
  [B | Stack1] = Stack,
  [A | Stack2] = Stack1,
  calculate(T, [math:pow(A, B) | Stack2]);

calculate([H | T], Stack) when H == "sin" ->
  [A | Stack1] = Stack,
  calculate(T, [math:sin(A) | Stack1]);

calculate([H | T], Stack) when H == "cos" ->
  [A | Stack1] = Stack,
  calculate(T, [math:cos(A) | Stack1]);

calculate([H | T], Stack) when H == "tan" ->
  [A | Stack1] = Stack,
  calculate(T, [math:tan(A) | Stack1]);

calculate([H | T], Stack) ->
  Number = list_to_float(H),
  calculate(T, [Number | Stack]).