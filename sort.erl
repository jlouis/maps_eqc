-module(sort).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

sort(L) ->
    lists:sort(fun(X, Y) -> erts_internal:cmp_term(X, Y) < 0 end, L).
    
prop_sorted() ->
    ?FORALL(L, maps_eqc:map_list(),
        begin
            Sorted = sort(L),
            conjunction([
              {size, equals(length(L), length(Sorted))},
              {ordering, ordered(Sorted)}
            ])
        end).
        
ordered([]) -> true;
ordered([_]) -> true;
ordered([X,Y|T]) ->
    case erts_internal:cmp_term(X,Y) < 0 of
        true -> ordered([X|T]);
        false -> false
    end.
