% Rosalind, problem RNA.
% 11/28/2014.

% GATGGAACTTGACTACGTAAATT
% GAUGGAACUUGACUACGUAAAUU

-module(rosalind_rna).
-export([start/0]).

replace_ut([84 | T]) -> [85] ++ replace_ut(T);
replace_ut([H | T]) -> [H] ++ replace_ut(T);
replace_ut([]) -> [].

start() -> io:format("~p", [replace_ut("GATGGAACTTGACTACGTAAATT")]).
