%%% Generate a list of lists with hashes colliding on the internal state.
%%%
-module(collisions).

-export([generate/0, save_state/0]).

-define(RANGE, 65536 * 65536).
-define(ITERATIONS, 65536 * 100).

save_state() ->
    Terms = generate(),
    file:write_file("priv/colliding_terms.term", io_lib:format("~p.", [Terms])).

generate() ->
    erts_debug:set_internal_state(available_internal_state, true),
    Table = ets:new(generator_table, [bag]),
    RandSeed = rand:seed_s(exs64, erlang:timestamp()),
    generate(Table, RandSeed),
    Collisions = find_collisions(Table),
    ets:delete(Table),
    Collisions.

generate(Table, RandSeed) ->
    {Populated, RS2} = populate(Table, RandSeed),
    iterate(Populated, RS2, ?ITERATIONS),
    ok.

populate(Table, RandSeed) ->
    case ets:info(Table, size) > 50000 of
        true -> {Table, RandSeed};
        false ->
            {I, NextSeed} = rand:uniform_s(?RANGE, RandSeed),
            Hash = internal_hash(I),
            ets:insert(Table, [{Hash, I}]),
            populate(Table, NextSeed)
    end.

iterate(_Table, _Seed, 0) -> ok;
iterate(Table, Seed, K) ->
    {I, NextSeed} = rand:uniform_s(?RANGE, Seed),
    Hash = internal_hash(I),
    case ets:member(Table, Hash) of
        true ->
            ets:insert(Table, {Hash, I}),
            ok;
        false ->
            ok
    end,
    iterate(Table, NextSeed, K-1).


find_collisions(Table) ->
    find_collisions(Table, ets:first(Table)).

find_collisions(_Table, '$end_of_table') -> [];
find_collisions(Table, Key) ->
    case ets:lookup_element(Table, Key, 2) of
        [_] -> find_collisions(Table, ets:next(Table, Key));
        L when length(L) > 1 -> [L | find_collisions(Table, ets:next(Table, Key))]
    end.

internal_hash(Term) ->
    erts_debug:get_internal_state({internal_hash, Term}).

