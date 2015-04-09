-module(maps_runner).
-behaviour(gen_server).

%% Standard boilerplate stuff
-export([
	ensure_started/1,
	start/0,
	start_link/0,
	reset/0, reset/1
]).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Meta-commands
-export([
	become/1,
	convert/0,
	eq/1,
	extract/0,
	illegal/2,
	recall/1,
	remember/1
]).

%% The real commands we can execute
-export([
	find/1,
	fold/2,
	is_key/1,
	keys/0,
	%% map/1, % Implemented directly in maps_eqc
	merge/1,
	m_get/1, m_get/2,
	populate/2,
	put/2,
	remove/1,
	size/0,
	to_list/0,
	update/2,
	values/0,
	with/1, with_q/1,
	without/1, without_q/1
]).

-record(state,{
	m :: map(), %% The map we are testing
	persist :: [{reference(), map()}] %% The persistence list
}).

ensure_started(local) ->
    case global:whereis_name(?MODULE) of
         undefined ->
             start_link(),
             reset(),
             ok;
         Pid when is_pid(Pid) ->
             reset(),
             ok
    end;
ensure_started(Node) ->
    ensure_started(Node, 10).
    
ensure_started(_Node, 0) -> exit(gave_up_starting);
ensure_started(Node, K) ->
    ReplyPid = self(),
    Pid = spawn(Node,
      fun() ->
        start(),
        timer:sleep(10),
        ReplyPid ! {started, self()}
      end),
    receive
        {started, Pid} -> ok
    after 500 ->
        ensure_started(Node, K-1)
    end,
    global:sync(),
    reset(),
    ok.

start() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

reset() -> call(reset).
reset(M) -> call({reset, M}).

extract() -> call(extract).
eq(M) -> call({eq, M}).
remember(Ref) -> call({remember, Ref}).
recall(Ref) -> call({recall, Ref}).
become(Ref) -> call({become, Ref}).
illegal(Cmd, Map) -> call({illegal, Cmd, Map}).
convert() -> call(convert).

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
merge(Isns) -> call({merge, Isns}).
fold(F, Init) -> call({fold, F, Init}).
with_q(Ks) -> call({with_q, Ks}).
with(Ks) -> call({with, Ks}).
without_q(Ks) -> call({without_q, Ks}).
without(Ks) -> call({without, Ks}).

call(X) ->
    gen_server:call({global, ?MODULE}, X).

init([]) ->
    {ok, #state { m = #{}, persist = [] } }.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_call({become, Ref}, _From, #state { persist = Ps } = State) ->
    {value, {Ref, Old}, Ps2} = lists:keytake(Ref, 1, Ps),
    {reply, ok, State#state { m = Old, persist = Ps2 }};
handle_call({remember, Ref}, _From, #state { m = M, persist = Ps } = State) ->
    {reply, ok, State#state { persist = lists:keystore(Ref, 1, Ps, {Ref, M}) } };
handle_call({recall, Ref}, _From, #state { persist = Ps } = State) ->
    {Ref, M} = lists:keyfind(Ref, 1, Ps),
    {reply, {ok, M}, State};
handle_call({illegal, Cmd, Map}, _From, State) ->
    try
        process(Cmd, Map),
        {reply, ok, State}
    catch
        Class:Reason ->
           {reply, {Class, Reason}, State}
    end;
handle_call(C, _From, #state { m = M } = State) ->
    {R, M2} = process(C, M),
    {reply, R, State#state { m = M2 }}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_Oldvsn, State, _Aux) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

process(convert, M) ->
    RoundTrip = binary_to_term( term_to_binary(M) ),
    {RoundTrip, M};
process(extract, M) -> {M, M};
process({eq, MIn}, M) ->
    {MIn =:= M, M};
process({reset, M}, _) -> {ok, M};
process(reset, _) -> {ok, maps:new()};
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
process({update_no_fail, K, V}, M) ->
    M2 = maps:update(K, V, M),
    {M2, M2};
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
process({get_no_fail, K}, M) -> {maps:get(K, M), M};
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
process({merge, Isns}, M) ->
    Res = case Isns of
    		identity -> maps:merge(M, M);
    		{left, M2} -> maps:merge(M2, M);
    		{right, M2} -> maps:merge(M, M2)
    end,
    {Res, Res};
process({fold, F, Init}, M) -> {maps:fold(F, Init, M), M};
process({with_q, Ks}, M) ->
    {maps:with(Ks, M), M};
process({with, Ks}, M) ->
    M2 = maps:with(Ks, M),
    {M2, M2};
process({without_q, Ks}, M) ->
    {maps:without(Ks, M), M};
process({without, Ks}, M) ->
    M2 = maps:without(Ks, M),
    {M2, M2};
process(_, M) -> {{error, unknown_call}, M}.

