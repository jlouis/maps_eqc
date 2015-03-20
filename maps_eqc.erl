-module(maps_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state,
    { contents = [] }).
    
initial_state() -> #state{}.

%% GENERATORS
map_key() -> int().
map_value() -> int().

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
      ?FORALL(Cmds, commands(?MODULE),
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
