% Rosalind, problem PRTM.
% 12/07/2014.

-module(rosalind_prtm).
-export([start/0]).
-import(rosalind_util_protein, [protein_list/1, protein_list_mass/1]).

start() ->
    NS = "SKADYEK",
    PL = protein_list(NS),
    io:format("~w", [protein_list_mass(PL)]).
