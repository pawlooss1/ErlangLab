%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2019 19:00
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("pawel").

-include_lib("eunit/include/eunit.hrl").

%% TESTS
% createMonitor/0
create_test() ->
  ?assertEqual({#{}, #{}},
    pollution:createMonitor()).

% addStation/2
add_first_test() ->
  ?assertEqual({#{"Wojtek" => {station, {1, 1}, "Wojtek", []}}, #{{1, 1} => "Wojtek"}},
    pollution:addStation({#{}, #{}}, "Wojtek", {1, 1})).
add_existing_coords_test() ->
  Monitor = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual(stationExists,
    pollution:addStation(Monitor, "Nowa Stacja?", {50.2345, 18.3445})).
add_existing_name_test() ->
  Monitor = pollution:addStation({#{}, #{}}, "Super Stacja", {5.543, 11.24}),
  ?assertEqual(stationExists,
    pollution:addStation(Monitor, "Super Stacja", {50.2345, 18.3445})).

% addValue/5
add_value_coords_test() ->
  Monitor = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual({#{"Super Stacja" =>{station, {50.2345, 18.3445}, "Super Stacja", [{measurement, "PM2.5", 12.5, {{2019, 5, 8}, {19, 23, 55}}}]}}, #{{50.2345, 18.3445} => "Super Stacja"}},
    pollution:addValue(Monitor, {50.2345, 18.3445}, {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 12.5)).
add_value_name_test() ->
  Monitor = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  ?assertEqual({#{"Super Stacja" =>{station, {50.2345, 18.3445}, "Super Stacja", [{measurement, "PM2.5", 12.5, {{2019, 5, 8}, {19, 23, 55}}}]}}, #{{50.2345, 18.3445} => "Super Stacja"}},
    pollution:addValue(Monitor, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 12.5)).
add_existing_measurement_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(M1,
    pollution:addValue(M1, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 80)).
add_to_non_existing_station() ->
  ?assertEqual(noSuchStation,
    pollution:addValue({#{}, #{}}, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 80)).

% removeValue/4
remove_value_coords_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(M,
    pollution:removeValue(M1, {50.2345, 18.3445}, {{2019, 5, 8}, {19, 23, 55}}, "PM10")).
remove_value_name_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(M,
    pollution:removeValue(M1, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10")).
remove_non_existing_value_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(M1,
    pollution:removeValue(M1, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5")).
remove_from_non_existing_station_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(noSuchStation,
    pollution:removeValue(M1, {10, 90}, {{2019, 5, 8}, {19, 23, 55}}, "PM2.5")).

% getOneValue/4
get_value_coords_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  ?assertEqual(1.5,
    pollution:getOneValue(M1, {50.2345, 18.3445}, {{2019, 5, 8}, {19, 23, 55}}, "PM10")).
get_value_name_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 20.7),
  ?assertEqual(20.7,
    pollution:getOneValue(M1, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5")).
get_non_existing_value_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 20.7),
  ?assertEqual(noMeasurement,
    pollution:getOneValue(M1, {50.2345, 18.3445}, {{2048, 5, 2}, {1, 23, 5}}, "PM2.5")).
get_from_non_existing_station_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 20.7),
  ?assertEqual(noSuchStation,
    pollution:getOneValue(M1, "Niesuper Stacja", {{2048, 5, 2}, {1, 23, 5}}, "PM2.5")).

% getStationMean/3
get_mean_coords_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {9, 23, 55}}, "PM10", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(5.0,
    pollution:getStationMean(M3, {50.2345, 18.3445}, "PM10")).
get_mean_name_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {9, 23, 55}}, "PM2.5", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(3.0,
    pollution:getStationMean(M3, "Super Stacja", "PM2.5")).
get_mean_with_no_measurements_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {9, 23, 55}}, "PM2.5", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM2.5", 9),
  ?assertEqual(noMeasurements,
    pollution:getStationMean(M3, "Super Stacja", "MP5.2")).
get_mean_from_not_existing_station_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM2.5", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {9, 23, 55}}, "PM2.5", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM2.5", 9),
  ?assertEqual(noSuchStation,
    pollution:getStationMean(M3, {4,20}, "PM2.5")).

% getDailyMean/3
get_daily_mean_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addStation(M, "Jeszcze lepsza stacja", {20.667, 9.97}),
  M2 = pollution:addValue(M1, "Jeszcze lepsza stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 2.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 5, 8}, {9, 23, 55}}, "PM10", 6.5),
  M4 = pollution:addValue(M3, "Super Stacja", {{2019, 5, 8}, {9, 13, 45}}, "PM10", 1.5),
  ?assertEqual(3.5,
    pollution:getDailyMean(M4, {2019, 5, 8}, "PM10")).
get_daily_mean_with_no_measurements_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addStation(M, "Jeszcze lepsza stacja", {20.667, 9.97}),
  M2 = pollution:addValue(M1, "Jeszcze lepsza stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 2.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 5, 8}, {9, 23, 55}}, "PM10", 6.5),
  M4 = pollution:addValue(M3, "Super Stacja", {{2019, 5, 8}, {9, 13, 45}}, "PM10", 1.5),
  ?assertEqual(noMeasurements,
    pollution:getDailyMean(M4, {119, 15, 9}, "PM10")).

% getHourlyMean/4
get_hourly_mean_coords_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {19, 23, 55}}, "PM10", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(3.0,
    pollution:getHourlyMean(M3, {50.2345, 18.3445}, {19, 23, 55}, "PM10")).
get_hourly_mean_name_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {19, 23, 55}}, "PM10", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(9.0,
    pollution:getHourlyMean(M3, "Super Stacja", {9, 13, 45}, "PM10")).
get_hourly_mean_with_no_measurements_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {19, 23, 55}}, "PM10", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(noMeasurements,
    pollution:getHourlyMean(M3, {50.2345, 18.3445}, {20, 0, 0}, "PM10")).
get_hourly_mean_from_not_existing_station_test() ->
  M = pollution:addStation({#{}, #{}}, "Super Stacja", {50.2345, 18.3445}),
  M1 = pollution:addValue(M, "Super Stacja", {{2019, 5, 8}, {19, 23, 55}}, "PM10", 1.5),
  M2 = pollution:addValue(M1, "Super Stacja", {{2019, 15, 8}, {19, 23, 55}}, "PM10", 4.5),
  M3 = pollution:addValue(M2, "Super Stacja", {{2019, 25, 8}, {9, 13, 45}}, "PM10", 9),
  ?assertEqual(noSuchStation,
    pollution:getHourlyMean(M3, {545, 0.3445}, {9, 13, 45}, "PM10")).