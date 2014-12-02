% Rosalind, problem RNA.
% 11/28/2014.

% GATGGAACTTGACTACGTAAATT
% GAUGGAACUUGACUACGUAAAUU

-module(rosalind_rna).
-export([start/0]).
-import(rosalind, [nucleobase_list/1, nucleobase_list_print/1]).

replace_nucleobases(L) -> replace_nucleobases(L, []).
replace_nucleobases([thymine | T], Acc) -> replace_nucleobases(T, Acc ++ [uracil]);
replace_nucleobases([H | T], Acc) -> replace_nucleobases(T, Acc ++ [H]);
replace_nucleobases([], Acc) -> Acc.

start() ->
    L = nucleobase_list("GATGGAACTTGACTACGTAAATT"),
    nucleobase_list_print(replace_nucleobases(L)).
