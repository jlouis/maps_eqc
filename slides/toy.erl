-module(toy).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_zip_inverse() ->
    ?FORALL(Arr, list(choose(0, 255)),
        begin
            Bin = list_to_binary(Arr),
            Z = zlib:uncompress(zlib:compress(Bin)),
            measure(array_length, length(Arr), equals(Bin, Z))
        end).
