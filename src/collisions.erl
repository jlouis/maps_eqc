%%% Generate a list of lists with hashes colliding on the internal state.
%%%
-module(collisions).

-export([generate/0, save_state/0]).

-define(k64, 65536).

-define(RANGE, ?k64 * ?k64 * ?k64).
-define(ITERATIONS, ?k64 * 2000).
-define(TBL_SIZE, ?k64 * 4).
-define(NUM_CORES, 8).

save_state() ->
    Terms = lists:sort(fun(L1, L2) -> length(L1) > length(L2) end, generate()),
    file:write_file("priv/colliding_terms.term", io_lib:format("~p.", [Terms])).

generate() ->
    erts_debug:set_internal_state(available_internal_state, true),
    Table = ets:new(generator_table, [bag, {read_concurrency, true}, public]),
    generate(Table),
    fanout(Table, ?NUM_CORES),
    Collisions = find_collisions(Table),
    ets:delete(Table),
    Collisions.

generate(Table) ->
    populate(Table),
    ok.

populate(Table) ->
    case ets:info(Table, size) > ?TBL_SIZE of
        true -> Table;
        false ->
            I = rand:uniform(?RANGE),
            Hash = internal_hash(I),
            ets:insert(Table, [{Hash, I}]),
            populate(Table)
    end.

fanout(Table, Cores) ->
    collect([spawn_monitor(fun() -> iterate_init(Table) end) || _ <- lists:seq(1, Cores)]).
    
collect([]) -> ok;
collect([{_Pid, Ref} | Monitored]) ->
   receive
       {'DOWN', Ref, _, _, normal} ->
           collect(Monitored);
       {'DOWN', Ref, _, _, Otherwise} ->
           exit(Otherwise)
   end.

iterate_init(Table) ->
    iterate(Table, ?ITERATIONS).

iterate(_Table, 0) -> ok;
iterate(Table, K) ->
    I = rand:uniform(?RANGE),
    Hash = internal_hash(I),
    case ets:member(Table, Hash) of
        true ->
            ets:insert(Table, {Hash, I}),
            ok;
        false ->
            ok
    end,
    iterate(Table, K-1).

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

