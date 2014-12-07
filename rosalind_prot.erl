% Rosalind, problem PROT.
% 12/06/2014.

-module(rosalind_prot).
-export([start/0]).
-import(rosalind_util_nucleobase, [nucleobase_list/1]).
-import(rosalind_util_protein, [protein_list_print/1, protein_list/1, protein_list_string/1]).

start() ->
    NS = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA",
    NL = nucleobase_list(NS),
    PL = protein_list(NL),
    protein_list_print(PL).
