%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 10:39
%%%-------------------------------------------------------------------
-module(makota).
-author("pawel").

%% API
-export([power/2]).

power(_, 0) ->
  1;

power(X, N) ->
  X * power(X, N-1).