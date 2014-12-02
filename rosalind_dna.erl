% Rosalind, problem DNA.
% 11/28/2014.

% AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
% 20 12 17 21

-module(rosalind_dna).
-export([start/0]).
-import(rosalind, [nucleobase_list/1, nucleobase_list_print/1]).

count_nucleobases(L) -> count_nucleobases(L, [0, 0, 0, 0]).
count_nucleobases([], C) -> C;
count_nucleobases([H | T], [Ac, Cc, Gc, Tc]) ->
    case H of
        adenine -> count_nucleobases(T, [Ac + 1, Cc, Gc, Tc]);
        cytosine -> count_nucleobases(T, [Ac, Cc + 1, Gc, Tc]);
        guanine -> count_nucleobases(T, [Ac, Cc, Gc + 1, Tc]);
        thymine -> count_nucleobases(T, [Ac, Cc, Gc, Tc + 1])
    end.

start() ->
    L = nucleobase_list("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"),
    io:format("~w", [count_nucleobases(L)]).
