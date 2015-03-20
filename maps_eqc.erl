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
%% merge/2
%% map/2
%% fold/3

%% GENERATORS
map_key() -> int().
map_value() -> int().

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
    [oneof([from_list, puts]), list({map_key(), map_value()})].
    
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

%% IS_KEY
%% --------------------------------------------------------------

is_key_pos(K) ->
    maps_runner:is_key(K).
    
is_key_pos_pre(#state { contents = C }) -> C /= [].

is_key_pos_args(#state { contents = C }) ->
    ?LET(Pair, elements(C),
        [element(1, Pair)]).

is_key_pos_return(_S, [_K]) ->
    true.

is_key_neg(K) ->
    maps_runner:is_key(K).
    
is_key_neg_args(#state { contents = C }) ->
    ?SUCHTHAT([K], [map_key()],
        lists:keyfind(K, 1, C) == false).

is_key_neg_pre(#state { contents = C}, [K]) ->
    lists:keyfind(K, 1, C) == false.

is_key_neg_return(_S, [_K]) ->
    false.

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

%% SIZE
%% --------------------------------------------------------------
size() ->
    maps_runner:size().
    
size_args(_S) -> [].

size_return(#state { contents = C }, []) ->
    length(C).
    
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
          aggregate(command_names(Cmds),
              pretty_commands(?MODULE, Cmds, {H,S,R}, R == ok))
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
