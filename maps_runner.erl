-module(maps_runner).
-behaviour(gen_server).

%% Standard boilerplate stuff
-export([start_link/0, reset/0]).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% The real commands we can execute
-export([
	find/1,
	is_key/1,
	keys/0,
	merge/1,
	m_get/1, m_get/2,
	populate/2,
	put/2,
	remove/1,
	size/0,
	to_list/0,
	update/2,
	values/0,
	map/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
reset() -> call(reset).
size() -> call(size).
put(K, V) -> call({put, K, V}).
is_key(K) -> call({is_key, K}).
keys() -> call(keys).
remove(K) -> call({remove, K}).
to_list() -> call(to_list).
update(K, V) -> call({update, K, V}).
values() -> call(values).
m_get(K) -> call({get, K}).
m_get(K, Def) -> call({get, K, Def}).
find(K) -> call({find, K}).
populate(Variant, Elems) -> call({populate, Variant, Elems}).
merge(M) -> call({merge, M}).
map(F) -> call({map, F}).

call(X) ->
    gen_server:call(?MODULE, X).

init([]) ->
    {ok, #{}}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_call(C, _From, State) ->
    {R, NS} = process(C, State),
    {reply, R, NS}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_Oldvsn, State, _Aux) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

process(reset, _) -> {ok, #{}};
process(size, M) -> {maps:size(M), M};
process({put, K, V}, M) ->
    M2 = maps:put(K, V, M),
    {M2, M2};
process({is_key, K}, M) -> {maps:is_key(K, M), M};
process(keys, M) -> {maps:keys(M), M};
process({remove, K}, M) ->
    M2 = maps:remove(K, M),
    {M2, M2};
process(to_list, M) -> {maps:to_list(M), M};
process({update, K, V}, M) ->
    try
        M2 = maps:update(K, V, M),
        {M2, M2}
    catch
        Class:Err -> {{Class,Err}, M}
    end;
process(values, M) -> {maps:values(M), M};
process({populate, from_list, L}, _) ->
   M = maps:from_list(L),
   {M, M};
process({populate, puts, L}, _) ->
   M = lists:foldl(fun({K, V}, M) -> maps:put(K, V, M) end, #{}, L),
   {M, M};
process({get, K}, M) ->
    try
        V = maps:get(K, M),
        {V, M}
    catch
        Class:Err -> {{Class, Err}, M}
    end;
process({get, K, Def}, M) ->
    V = maps:get(K, M, Def),
    {V, M};
process({find, K}, M) ->
    Res = maps:find(K, M),
    {Res, M};
process({merge, M2}, M) ->
    Res = maps:merge(M, M2),
    {Res, Res};
process({map, F}, M) ->
    M2 = maps:map(F, M),
    {M2, M2};
process(_, M) -> {{error, unknown_call}, M}.

