-module(maps_runner).
-behaviour(gen_server).

%% Standard boilerplate stuff
-export([start_link/0, reset/0]).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% The real commands we can execute
-export([
	size/0,
	put/2,
	is_key/1,
	keys/0,
	remove/1,
	to_list/0,
	update/2,
	values/0
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
    M2 = maps:update(K, V, M),
    {M2, M2};
process(values, M) -> {maps:values(M), M};
process(_, M) -> {{error, unknown_call}, M}.

