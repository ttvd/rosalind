% Rosalind, problem DNA.
% 11/28/2014.

% AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
% A C G T

% 65 - A, 67 - C,  71 - G, 84 - T

-module(dna).
-export([start/0]).

count_agct([65 | T], [Ac, Cc, Gc, Tc]) -> count_agct(T, [Ac + 1, Cc, Gc, Tc]);
count_agct([67 | T], [Ac, Cc, Gc, Tc]) -> count_agct(T, [Ac, Cc + 1, Gc, Tc]);
count_agct([71 | T], [Ac, Cc, Gc, Tc]) -> count_agct(T, [Ac, Cc, Gc + 1, Tc]);
count_agct([84 | T], [Ac, Cc, Gc, Tc]) -> count_agct(T, [Ac, Cc, Gc, Tc + 1]);
count_agct([], C) -> C.

start() -> io:format("~w", [count_agct("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC", [0, 0, 0, 0])]).
