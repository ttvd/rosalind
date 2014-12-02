% Rosalind, problem LEXF.
% 12/01/2014.

% T A G C, 2

-module(rosalind_lexf).
-export([start/0]).

generate_lexf(_, A, 1) -> A;
generate_lexf(L, A, C) ->
    N = [[H] ++ [T] || H <- L, T <- A],
    generate_lexf(L, N, C - 1).

print_lexf([H | T]) ->
    io:format("~s~n", [H]),
    print_lexf(T);
print_lexf([]) -> ok.

start() ->
    S = "TAGC",
    L = generate_lexf(S, S, 2),
    print_lexf(L).
