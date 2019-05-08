%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2019 19:00
%%%-------------------------------------------------------------------
-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-author("pawel").

%% TESTS

create_test() ->
  ?assertEqual({#{},#{}},
               pollution:createMonitor()).

add_first_test() ->
  ?assertEqual({#{"Wojtek" => {station,{1,1},"Wojtek",[]}}, #{{1,1} => "Wojtek"}},
               pollution:addStation({#{},#{}}, "Wojtek", {1,1})).
add_existing_coords_test() ->
  Monitor = pollution:addStation({#{},#{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual(Monitor,
               pollution:addStation(Monitor, "Nowa Stacja?", {50.2345, 18.3445})).
add_existing_name_test() ->
  Monitor = pollution:addStation({#{},#{}}, "Super Stacja", {5.543, 11.24}),
  ?assertEqual(Monitor,
               pollution:addStation(Monitor, "Super Stacja", {50.2345, 18.3445})).

add_value_coords_test() ->
  Monitor = pollution:addStation({#{},#{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual({#{"Super Stacja" =>{station,{50.2345, 18.3445}, "Super Stacja", [{measurement,"PM2.5",12.5,{{2019,5,8},{19,23,55}}}]}}, #{{50.2345, 18.3445} => "Super Stacja"}},
               pollution:addValue(Monitor, {50.2345, 18.3445}, {{2019,5,8},{19,23,55}}, "PM2.5", 12.5)).
add_value_name_test() ->
  Monitor = pollution:addStation({#{},#{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual({#{"Super Stacja" =>{station,{50.2345, 18.3445}, "Super Stacja", [{measurement,"PM2.5",12.5,{{2019,5,8},{19,23,55}}}]}}, #{{50.2345, 18.3445} => "Super Stacja"}},
               pollution:addValue(Monitor, "Super Stacja", {{2019,5,8},{19,23,55}}, "PM2.5", 12.5)).
add_existing_measurement_test() ->
  M = pollution:addStation({#{},#{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019,5,8},{19,23,55}}, "PM10", 1.5),
  ?assertEqual(M1,
               pollution:addValue(M1, "Super Stacja", {{2019,5,8},{19,23,55}}, "PM10", 80)).
add_to_not_existing_station() ->
  ?assertEqual(noSuchStation,
               pollution:addValue({#{},#{}}, "Super Stacja", {{2019,5,8},{19,23,55}}, "PM10", 80)).

remove_value_coords_test() ->
  M = pollution:addStation({#{},#{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019,5,8},{19,23,55}}, "PM10", 1.5),
  ?assertEqual(M,
               pollution:addValue(M1, {50.2345, 18.3445}, {{2019,5,8},{19,23,55}}, "PM10")).