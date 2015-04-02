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
        
large_map(_N, 0, _RandState, Acc) -> Acc;
large_map(N, K, RandState, Acc) ->
    {Key, RS1} = rand:uniform_s(N, RandState),
    {Value, RS2} = rand:uniform_s(N, RS1),
    large_map(N, K-1, RS2, [{Key, Value} | Acc]).

prop_lm_iso_fg() ->
    ?FORALL(RS, rand_seed(exs64),
       begin
          Map = maps:from_list(large_map(65536*65536, 25000, RS, [])),
          L = maps:to_list(Map),
          Map2 = maps:from_list(L),
          Map =:= Map2
       end).

