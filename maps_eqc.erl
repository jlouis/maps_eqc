-module(maps_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state,
    { contents = [] }).
    
initial_state() -> #state{}.

%%% MISSING:
%% with/2
%% without/2
%% fold/3

%% GENERATORS
map_key() -> int().
map_value() -> int().

%% MAP/2
%% --------------------------------------------------------------

map(F) ->
     Res = maps_runner:map(F),
     lists:sort(maps:to_list(Res)).
     
map_args(_S) ->
     [function2(map_value())].
     
map_next(#state { contents = Cs } = State, _, [F]) ->
    NCs = [{K, F(K, V)} || {K, V} <- Cs],
    State#state { contents = NCs }.
    
map_return(#state { contents = Cs }, [F]) ->
    NCs = [{K, F(K, V)} || {K, V} <- Cs],
    lists:sort(NCs).
    
map_features(_S, _, _) -> ["R026: using the map/2 functor on the map()"].

%% MERGE/2
%% --------------------------------------------------------------

merge(M) ->
    Res = maps_runner:merge(M),
    lists:sort(maps:to_list(Res)).
    
merge_args(_S) ->
    ?LET(Elems, list({map_key(), map_value()}),
        [maps:from_list(Elems)]).
        
merge_next(#state { contents = C } = State, _, [M]) ->
    NC = maps:fold(fun (K, V, Cs) -> lists:keystore(K, 1, Cs, {K, V}) end, C, M),
    State#state { contents = NC }.

merge_return(#state { contents = C }, [M]) ->
    Res = maps:fold(fun (K, V, Cs) -> lists:keystore(K, 1, Cs, {K, V}) end, C, M),
    lists:sort(Res).

merge_features(_S, _, _) ->
    ["R019: Merging two maps"].

%% FIND/2
%% --------------------------------------------------------------

find(K) ->
    maps_runner:find(K).

find_args(#state { contents = C } = S) ->
    frequency(
       [{5, [random_key(S)]} || C /= []] ++
       [{1, ?SUCHTHAT([K], [map_key()], lists:keyfind(K,1,C) == false)} ]).


find_return(#state { contents = C }, [K]) ->
    case lists:keyfind(K, 1, C) of
         false -> error;
         {K, V} -> {ok, V}
    end.

find_features(_S, _, error) -> ["R020: find on a non-existing key"];
find_features(_S, _, {ok, _V}) -> ["R021: find on an existing key"].

%% GET/3
%% --------------------------------------------------------------

m_get_default(K, Default) ->
    maps_runner:m_get(K, Default).

m_get_default_args(#state { contents = C }) ->
    frequency(
      [{5, ?LET({Pair, Default}, {elements(C), make_ref()}, [element(1, Pair), Default])} || C /= [] ] ++
      [{1, ?SUCHTHAT([K, _Default], [map_key(), make_ref()], lists:keyfind(K, 1, C) == false)}]).

m_get_default_return(#state { contents = C }, [K, Default]) ->
    case lists:keyfind(K,1,C) of
       false -> Default;
       {K, V} -> V
    end.

m_get_default_features(S, [K, _Default], _) ->
    case member(K, S) of
         true -> ["R024: get/3 on an existing key"];
         false -> ["R025: get/3 on a non-existing key"]
    end.

%% GET/2
%% --------------------------------------------------------------

m_get(K) ->
    maps_runner:m_get(K).

m_get_args(#state { contents = C }) ->
    frequency(
      [{5, ?LET(Pair, elements(C), [element(1, Pair)])} || C /= []] ++
      [{1, ?SUCHTHAT([K], [map_key()], lists:keyfind(K,1,C) == false)}]).

m_get_return(#state { contents = C }, [K]) ->
    case lists:keyfind(K,1,C) of
       false -> {error, bad_key};
       {K, V} -> V
    end.

m_get_features(_S, _, {error, bad_key}) -> ["R022: get/2 on a non-existing key"];
m_get_features(_S, _, _) -> ["R023: get on a successful key"].

%% POPULATE
%% --------------------------------------------------------------

populate(_K, Variant, Elems) ->
    M = maps_runner:populate(Variant, Elems),
    lists:sort(maps:to_list(M)).

populate_pre(#state { contents = C }) -> C == [].

populate_args(_S) ->
  ?LET({Variant, K}, {oneof([from_list, puts]), oneof([nat(), 8, 16, 64, 256])},
      [K, Variant, vector(K, {map_key(), map_value()})]).
    
populate_next(State, _, [_K, _Variant, Elems]) ->
    Contents = lists:foldl(fun({K, V}, M) -> add_contents(K, V, M) end, [], Elems),
    State#state { contents = Contents }.

populate_return(_State, [_K, _Variant, Elems]) ->
    Contents = lists:foldl(fun({K, V}, M) -> add_contents(K, V, M) end, [], Elems),
    lists:sort(Contents).

populate_features(_S, [K, Variant, _], _) ->
    Sz = interpret_size(K),
    case Variant of
        from_list -> ["R017x: populating an empty map with from_list/1 of size: " ++ Sz];
        puts -> ["R018x: populating an empty map with put/2 size: " ++ Sz]
    end.
    
interpret_size(256) -> "256";
interpret_size(64) -> "64";
interpret_size(16) -> "16";
interpret_size(8) -> "8";
interpret_size(_Sz) -> "nat()".

%% VALUES
%% --------------------------------------------------------------

values() ->
    lists:sort(maps_runner:values()).
    
values_args(_S) -> [].

values_return(#state { contents = C }, []) ->
    lists:sort([V || {_, V} <- C]).

values_features(_S, _, _) ->
    ["R014: values/1 called on map"].

%% UPDATE
%% --------------------------------------------------------------


update(K, V) ->
    case maps_runner:update(K, V) of
        {error, Reason} -> {error, Reason};
        M -> lists:sort(maps:to_list(M))
    end.
    
update_args(#state { contents = C }) ->
    frequency(
      [{5, ?LET(Pair, elements(C), [element(1, Pair), map_value()])} || C /= [] ] ++
      [{1,  ?SUCHTHAT([K, _V], [map_key(), map_value()], lists:keyfind(K, 1, C) == false)}]).
        
update_next(#state { contents = C } = State, _, [K, V]) ->
    State#state { contents = replace_contents(K, V, C) }.

update_return(#state { contents = C} = S, [K, V]) ->
    case member(K, S) of
        true -> lists:sort(replace_contents(K, V, C));
        false -> {error, badarg}
    end.

update_features(S, [K, _], _) ->
   case member(K, S) of
        true -> ["R015: update/3 on an existing key"];
        false -> ["R016: update/3 on a non-existing key"]
   end.

%% TO_LIST
%% --------------------------------------------------------------

to_list() ->
    L  = maps_runner:to_list(),
    lists:sort(L).
    
to_list_args(_S) -> [].

to_list_return(#state { contents = C }, []) ->
    lists:sort(C).

to_list_features(_, _, _) ->
    ["R013: to_list/1 called on map"].
    
%% REMOVE
%% --------------------------------------------------------------

remove(K) ->
    ResMap = maps_runner:remove(K),
    lists:sort(maps:to_list(ResMap)).
    
remove_args(#state { contents = C }) ->
    frequency(
      [{5, ?LET(Pair, elements(C), [element(1, Pair)])} || C /= [] ] ++
      [{1,  ?SUCHTHAT([K], [map_key()], lists:keyfind(K, 1, C) == false)}]).
        
remove_next(#state { contents = C } = State, _, [K]) ->
    State#state { contents = del_contents(K, C) }.

remove_return(#state { contents = C }, [K]) ->
    lists:sort(del_contents(K, C)).

remove_features(S, [K], _) ->
    case member(K, S) of
       true -> ["R011: remove/2 of present key"];
       false -> ["R012: remove/2 of non-present key"]
    end.

%% KEYS
%% --------------------------------------------------------------

keys() ->
    lists:sort(maps_runner:keys()).
    
keys_args(_S) -> [].

keys_return(#state { contents = C }, []) ->
    lists:sort([K || {K, _} <- C]).

keys_features(_S, _, _) -> ["R010: Calling keys/1 on the map"].

%% IS_KEY
%% --------------------------------------------------------------

is_key(K) ->
    maps_runner:is_key(K).
    
is_key_args(#state { contents = C }) ->
    frequency(
        [{10, ?LET(Pair, elements(C), [element(1, Pair)])} || C /= [] ] ++
        [{1, ?SUCHTHAT([K], [map_key()], lists:keyfind(K, 1, C) == false)}]).

is_key_return(S, [K]) ->
    member(K, S).

is_key_features(_S, [_K], true) -> ["R001: is_key/2 on a present key"];
is_key_features(_S, [_K], false) -> ["R002: is_key/2 on a non-existing key"].

%% PUT
%% --------------------------------------------------------------

put(Key, Value) ->
    NewMap = maps_runner:put(Key, Value),
    lists:sort(maps:to_list(NewMap)).
    
put_args(_S) ->
    [map_key(), map_value()].

put_next(#state { contents = C } = State, _, [K, V]) ->
    State#state { contents = add_contents(K, V, C) }.

put_return(#state { contents = C}, [K, V]) ->
    lists:sort(add_contents(K, V, C)).

put_features(S, [K, _Value], _Res) ->
    case member(K, S) of
        true -> ["R003: put/3 on existing key"];
        false -> ["R004: put on a new key"]
    end.

%% SIZE
%% --------------------------------------------------------------
size() ->
    maps_runner:size().
    
size_args(_S) -> [].

size_return(#state { contents = C }, []) ->
    length(C).
    
size_features(_S, [], Sz) ->
   if
     Sz >= 128 -> ["R005: size/1 on a 128+ map"];
     Sz >= 64 -> ["R006: size/1 on a 64+ map"];
     Sz >= 16 -> ["R007: size/1 on a 16+ map"];
     Sz == 0 -> ["R008: size/1 on an empty (0) map"];
     true -> ["R009: size/1 on a small non-empty map"]
   end.

%% WEIGHT
%% -------------------------------------------------------------
weight(_S, populate) ->20;
weight(_S, _) -> 1.

%% PROPERTY
%% -------------------------------------------------------------
postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

prop_map() ->
    ?SETUP(fun() ->
        {ok, Pid} = maps_runner:start_link(),
        fun() -> exit(Pid, kill) end
    end,
      ?FORALL(Cmds, more_commands(8, commands(?MODULE)),
        begin
          maps_runner:reset(),
          {H,S,R} = run_commands(?MODULE, Cmds),
          aggregate(with_title('Commands'), command_names(Cmds),
          aggregate(with_title('Features'), call_features(H),
              pretty_commands(?MODULE, Cmds, {H,S,R}, R == ok)))
        end)).

%% HELPER ROUTINES
%% -------------------------------------------------------------
add_contents(K, V, C) ->
    lists:keystore(K, 1, C, {K, V}).

del_contents(K, C) ->
    lists:keydelete(K, 1, C).

replace_contents(K, V, C) ->
    lists:keyreplace(K, 1, C, {K, V}).

member(K, #state { contents = C }) ->
    lists:keymember(K, 1, C).

random_key(#state { contents = C }) ->
    ?LET(Pair, elements(C),
        element(1, Pair)).
