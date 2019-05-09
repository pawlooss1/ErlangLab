%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 09:54
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("pawel").

%% API
-export([start/0]).
-export([init/0, supervise/0]).

start() ->
  spawn(pollution_server_sup, init, []).

init() ->
  process_flag(trap_exit, true),
  supervise().

supervise() ->
  pollution_server:start(),
  receive
    {'EXIT', _, _} ->
      supervise()
  end.