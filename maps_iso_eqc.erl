%%% Simple isomorphic rules for maps
%%% Grafted as simple stateless tests for maps of different kinds
%%% The functions here will test most conversions to/from maps and make
%%% sure they work as expected.
-module(maps_iso_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% Generate an options list for term_to_binary/2
opts() ->
  ?LET({Compressed, MinorVersion},
    {oneof([[], [compressed], [{compressed, choose(0,9)}]]),
     oneof([[], [{minor_version, choose(0,1)}]])},
       Compressed ++ MinorVersion).

%% ISOMORPHISM over Lists
%%
%% A map() value is isomorphic to a list representation with
%% f: maps:to_list/1, and
%% g: maps:from_list/1
%%
%% Rules are that f·g is the identity and that g·f is the identity
%% The following two properties test for this
prop_list_iso_fg() ->
    ?FORALL(M,
      oneof([
          eqc_gen:map(maps_eqc:map_key(), maps_eqc:map_value()),
          maps_eqc:map_map()]),
        begin
            List = maps:to_list(M),
            M2 = maps:from_list(List),
            eqc_statem:eq(M, M2)
        end).

prop_list_iso_gf() ->
    ?FORALL(L, maps_eqc:map_list(),
      begin
        LD = dedup(L),
        M = maps:from_list(L),
        LD2 = maps:to_list(M),
        eqc_statem:eq(lists:sort(LD), lists:sort(LD2))
      end).

%% There is a binary embedding as well, but we only test the 'f·g' path
%% here since it is quite beastly to construct a valid binary out of nothing
prop_binary_iso_fg() ->
    ?FORALL([Opts, M],
      [opts(),
       oneof([
           eqc_gen:map(maps_eqc:map_key(), maps_eqc:map_value()),
           maps_eqc:map_map()]) ],
         begin
           Binary = term_to_binary(M, Opts),
           M2 = binary_to_term(Binary),
           eqc_statem:eq(M, M2)
         end).

%% Merged maps have certain correctness properties. In particular, the 2nd
%% map wins in key-conflict tournaments.
prop_merge() ->
    ?FORALL([M1, M2], [maps_eqc:map_map(), maps_eqc:map_map()],
      begin
        Merged = maps:merge(M1, M2),
        maps:fold(fun(K, V, Valid) ->
              case maps:find(K, M2) of
                {ok, V} -> Valid;
                {ok, _} -> false;
                error ->
                  case maps:find(K, M1) of
                    {ok, V} -> Valid;
                    {ok, _} -> false;
                    error -> false
                  end
              end
            end,
            true,
            Merged)
      end).

%% Deduplicate a list
is_key(_K, []) -> false;
is_key(K, [{K, _} | _]) -> true;
is_key(K, [_|T]) -> is_key(K, T).

dedup(L) ->
    lists:foldr(fun({K, V}, D) ->
    	case is_key(K, D) of
    	    true -> D;
    	    false -> [{K, V} | D]
    	end
     end,
     [],
     L).
