-module(phash2_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% phash2/1 consistency
%%
%% phash2/1 is consistent if elements which compare equal have the same hash value.
prop_phash2_consistent() ->
    ?FORALL(L, list(maps_eqc:map_term()),
        begin
            consistent(eqc_lib:sort(L))
        end).
        
%% A sorted list is consistent if equal elements next to each other are consistent
consistent([]) -> true;
consistent([_X]) -> true;
consistent([X, Y | XYs]) when X =:= Y ->
    case erlang:phash2(X) =:= erlang:phash2(Y) of
        true -> consistent([Y | XYs]);
        false -> {X, '/=', Y}
    end;
consistent([_X | XYs]) -> consistent(XYs).

