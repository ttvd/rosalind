% Rosalind, problem HAMM.
% 11/30/2014.

% GAGCCTACTAACGGGAT
% CATCGTAATGACGGCCT
% 7

-module(rosalind_hamm).
-export([start/0]).

hamming_distance([H1 | T1], [H2 | T2], C) ->
    case H1 =/= H2 of
        true -> hamming_distance(T1, T2, C + 1);
        false -> hamming_distance(T1, T2, C)
    end;
hamming_distance([], [], C) -> C.

start() -> io:format("~w", [hamming_distance("GAGCCTACTAACGGGAT", "CATCGTAATGACGGCCT", 0)]).
