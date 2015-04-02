-module(maps_large_iso_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% Simple isomorphism property on large maps
prop_lm_iso_fg() ->
    ?FORALL({RS, FK, FV}, {maps_eqc:rand_seed(exs64), function1(maps_eqc:map_key()), function1(maps_eqc:map_value())},
       begin
          Map = maps:from_list(maps_eqc:large_map(RS, 25000, 65536*65536, FK, FV)),
          L = maps:to_list(Map),
          Map2 = maps:from_list(L),
          Map =:= Map2
       end).

%% Simple isomorphism property on large maps, but keep all keys as integers.
prop_lm_iso_fg_simple() ->
    ?FORALL(RS, maps_eqc:rand_seed(exs64),
       begin
          Map = maps:from_list(maps_eqc:large_map(RS, 25000, 65536*65536)),
          L = maps:to_list(Map),
          Map2 = maps:from_list(L),
          Map =:= Map2
       end).
    
