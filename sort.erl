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
    case cmp_term(X,Y) of
        true -> ordered([X|T]);
        false -> false
    end.

%% The following implement term comparison in Erlang to test an alternative implementation
%% of erts_internal:cmp_term/2 
cmp_term(T1, T2) when is_integer(T1), is_integer(T2) -> T1 < T2;
cmp_term(T1, _) when is_integer(T1) -> true;
cmp_term(T1, T2) when is_float(T1), is_float(T2) -> T1 < T2;
cmp_term(T1, _) when is_float(T1) -> true;
cmp_term(T1, T2) when is_atom(T1), is_atom(T2) -> T1 < T2;
cmp_term(T1, _) when is_atom(T1) -> true;
cmp_term(T1, T2) when is_reference(T1), is_reference(T2) -> T1 < T2;
cmp_term(T1, _) when is_reference(T1) -> true;
cmp_term(T1, T2) when is_function(T1), is_function(T2) -> T1 < T2;
cmp_term(T1, _) when is_function(T1) -> true;
cmp_term(T1, T2) when is_port(T1), is_port(T2) -> T1 < T2;
cmp_term(T1, _) when is_port(T1) -> true;
cmp_term(T1, T2) when is_pid(T1), is_pid(T2) -> T1 < T2;
cmp_term(T1, _) when is_pid(T1) -> true;
cmp_term(T1, T2) when is_tuple(T1), is_tuple(T2) -> cmp_term(tuple_to_list(T1), tuple_to_list(T2));
cmp_term(T1, _) when is_tuple(T1) -> true;
cmp_term(T1, T2) when is_list(T1), is_list(T2) -> cmp_term_list(T1, T2);
cmp_term(T1, _) when is_list(T1) -> true;
cmp_term(T1, T2) when is_bitstring(T1), is_bitstring(T2) -> T1 < T2;
cmp_term(_, _) -> false.

cmp_term_list([], []) -> false;
cmp_term_list([], _) -> true;
cmp_term_list(_, []) -> false;
cmp_term_list([X|Xs], [Y|Ys]) when X =:= Y -> cmp_term_list(Xs, Ys);
cmp_term_list([X|_], [Y|_]) -> cmp_term(X, Y).