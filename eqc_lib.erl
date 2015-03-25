%%% @doc Erlang QuickCheck library functions
%%% Kept as one big module for ease of development.
%%% @end
-module(eqc_lib).
-vsn("1.0.0").
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%%% BIT INTEGERS
%%% ---------------------------------------------------------------
%%%

%% @doc pow_2_int/0 generates integers close to a power of two
%% It turns out that integers around powers of two are often able to mess up stuff
%% because of their bitwise representation. This generator generates integers close
%% to a power of two deliberately.
%% @end
pow_2_int() ->
    ?LET({Sign, Exponent, Perturb}, {sign(), choose(0, 128), choose(-3, 3)},
        Sign * pow(2, Exponent) + Perturb).

sign() -> elements([1, -1]).

pow(0, 0) -> 0;
pow(_Base, 0) -> 1;
pow(Base, N) -> Base * pow(Base, N-1).

%%% HEX STRING
%%% ---------------------------------------------------------------

%% @doc hex_char() generates a hexadecimal character
%% @end
hex_char() ->
    elements([$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $a, $b, $c, $d, $e, $f]).

%% @doc hex_string/0 generates a hex string
%% @end
hex_string() -> list(hex_char()).

%% @doc hex_string/1 generates a hexadecimal string of length `N'
%% @end
hex_string(N) ->
    vector(N, hex_char()).

%%% UUID
%%% ---------------------------------------------------------------

%% @doc uuid_v4() generates a v4 UUID
%% @end
uuid_v4() ->
    ?LET(
        {S1, S2, S3, S4, S5},
        {hex_string(8), hex_string(4), hex_string(3), hex_string(3), hex_string(12)},
            iolist_to_binary([S1, $-, S2, $-, $4, S3, $-, $a, S4, $-, S5])).

%%% SORTING
%%% ---------------------------------------------------------------
%%%

%% @doc sort/1 is a total sort function
%% The built-in lists:sort/1 is not total, because 0 == 0.0. Since the sort function
%% is also *stable* it can't be used to force a unique order on terms. This variant
%% of sort has the property of total order with INTEGER < FLOAT.
%% @end
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