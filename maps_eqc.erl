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
%% map/2
%% fold/3

%% GENERATORS
map_key() -> int().
map_value() -> int().

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

%% FIND/2
%% --------------------------------------------------------------

find_pos(K) ->
    maps_runner:find(K).
    
find_pos_pre(#state { contents = C }) ->
    C /= [].
    
find_pos_args(S) ->
    [random_key(S)].

find_pos_return(#state { contents = C }, [K]) ->
    {K, V} = lists:keyfind(K, 1, C),
    {ok, V}.

find_neg(K) ->
    maps_runner:find(K).
    
find_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K], [map_key()],
        lists:keyfind(K, 1, C) == false).

find_neg_return(_S, [_K]) ->
    error.

%% GET/3
%% --------------------------------------------------------------

m_get_default_pos(K, Default) ->
    maps_runner:m_get(K, Default).

m_get_default_pos_pre(#state { contents = C }) -> C /= [].

m_get_default_pos_args(#state { contents = C }) ->
    ?LET({Pair, Default}, {elements(C), make_ref()},
       [element(1, Pair), Default]).

m_get_default_pos_return(#state { contents = C }, [K, _Default]) ->
    {K, V} = lists:keyfind(K, 1, C),
    V.

m_get_default_neg(K, Default) ->
    maps_runner:m_get(K, Default).
    
m_get_default_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K, _Default], [map_key(), make_ref()],
        lists:keyfind(K, 1, C) == false).
        
m_get_default_neg_return(_S, [_K, Default]) ->
    Default.

%% GET/2
%% --------------------------------------------------------------

m_get_pos(K) ->
    maps_runner:m_get(K).

m_get_pos_pre(#state { contents = C }) -> C /= [].

m_get_pos_args(#state { contents = C }) ->
    ?LET(Pair, elements(C),
       [element(1, Pair)]).

m_get_pos_return(#state { contents = C }, [K]) ->
    {K, V} = lists:keyfind(K, 1, C),
    V.

m_get_neg(K) ->
    maps_runner:m_get(K).
    
m_get_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K], [map_key()],
        lists:keyfind(K, 1, C) == false).
        
m_get_neg_return(_S, [_K]) ->
    {error, bad_key}.
    
%% POPULATE
%% --------------------------------------------------------------

populate(Variant, Elems) ->
    M = maps_runner:populate(Variant, Elems),
    lists:sort(maps:to_list(M)).

populate_pre(#state { contents = C }) -> C == [].

populate_args(_S) ->
  ?LET({Variant, K}, {oneof([from_list, puts]), oneof([4, 16, 64, 256, nat()])},
      [Variant, vector(K, {map_key(), map_value()})]).
    
populate_next(State, _, [_Variant, Elems]) ->
    Contents = lists:foldl(fun({K, V}, M) -> add_contents(K, V, M) end, [], Elems),
    State#state { contents = Contents }.

populate_return(_State, [_Variant, Elems]) ->
    Contents = lists:foldl(fun({K, V}, M) -> add_contents(K, V, M) end, [], Elems),
    lists:sort(Contents).

%% VALUES
%% --------------------------------------------------------------

values() ->
    lists:sort(maps_runner:values()).
    
values_args(_S) -> [].

values_return(#state { contents = C }, []) ->
    lists:sort([V || {_, V} <- C]).

%% UPDATE
%% --------------------------------------------------------------

update_pos(K, V) ->
    M2 = maps_runner:update(K, V),
    lists:sort(maps:to_list(M2)).
    
update_pos_pre(#state { contents = C }) -> C /= [].

update_pos_args(#state { contents = C }) ->
    ?LET(Pair, elements(C),
        [element(1, Pair), map_value()]).
        
update_pos_next(#state { contents = C } = State, _, [K, V]) ->
    State#state { contents = replace_contents(K, V, C) }.

update_pos_return(#state { contents = C}, [K, V]) ->
    lists:sort(replace_contents(K, V, C)).

update_neg(K, V) ->
    maps_runner:update(K, V).
    
update_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K, _V], [map_key(), map_value()],
        lists:keyfind(K, 1, C) == false).

update_neg_return(_S, [_K, _V]) ->
    {error, badarg}.

%% TO_LIST
%% --------------------------------------------------------------

to_list() ->
    L  = maps_runner:to_list(),
    lists:sort(L).
    
to_list_args(_S) -> [].

to_list_return(#state { contents = C }, []) ->
    lists:sort(C).

%% REMOVE
%% --------------------------------------------------------------

remove_pos(K) ->
    ResMap = maps_runner:remove(K),
    lists:sort(maps:to_list(ResMap)).
    
remove_pos_pre(#state { contents = C }) -> C /= [].

remove_pos_args(#state { contents = C }) ->
    ?LET(Pair, elements(C),
        [element(1, Pair)]).
        
remove_pos_next(#state { contents = C } = State, _, [K]) ->
    State#state { contents = del_contents(K, C) }.

remove_pos_return(#state { contents = C }, [K]) ->
    lists:sort(del_contents(K, C)).

remove_neg(K) ->
    ResMap = maps_runner:remove(K),
    lists:sort(maps:to_list(ResMap)).
    
remove_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K], [map_key()],
        lists:keyfind(K, 1, C) == false).
        
remove_neg_pre(#state { contents = C }, [K]) ->
    lists:keyfind(K,1,C) == false.

remove_neg_return(#state { contents = C }, [_K]) ->
    lists:sort(C).

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
    lists:keymember(K, 1, S#state.contents).

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
    case lists:keymember(K, 1, S#state.contents) of
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
weight(_S, populate) -> 10;
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

random_key(#state { contents = C }) ->
    ?LET(Pair, elements(C),
        element(1, Pair)).
