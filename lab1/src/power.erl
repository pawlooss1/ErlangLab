%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2019 14:09
%%%-------------------------------------------------------------------
-module(power).
-author("pawel").

%% API
-export([power/2]).

power(_, 0) ->
  1;

power(X, N) ->
  X * power(X, N-1).