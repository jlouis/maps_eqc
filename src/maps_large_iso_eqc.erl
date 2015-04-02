-module(maps_large_iso_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

rand_seed() ->
    %% Should really be integers, but we shouldn't care.
    %% It never makes sense to shrink these down at all,
    %% so request we never shrink these values.
    noshrink({nat(), nat(), nat()}).
    
rand_seed(Alg) ->
    ?LET(Seed, rand_seed(),
        rand:seed_s(Alg, Seed)).
        
large_map(_N, 0, _FK, _FV, _RandState, Acc) -> Acc;
large_map(N, K, FK, FV, RandState, Acc) ->
    {KK, RS1} = rand:uniform_s(N, RandState),
    {KV, RS2} = rand:uniform_s(N, RS1),
    large_map(N, K-1, FK, FV, RS2, [{FK(KK), FV(KV)} | Acc]).

prop_lm_iso_fg() ->
    ?FORALL({RS, FK, FV}, {rand_seed(exs64), function1(maps_eqc:map_key()), function1(maps_eqc:map_value())},
       begin
          Map = maps:from_list(large_map(65536*65536, 25000, FK, FV, RS, [])),
          L = maps:to_list(Map),
          Map2 = maps:from_list(L),
          Map =:= Map2
       end).

