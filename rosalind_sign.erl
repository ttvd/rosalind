% Rosalind, problem SIGN.
% 12/01/2014.

-module(rosalind_sign).
-export([start/0]).

permutations([]) -> [[]];
permutations(L)  -> [[H | T] || H <- L, T <- permutations (L -- [H])].

signed_seq_list(0) -> [0];
signed_seq_list(N) -> [-N] ++ signed_seq_list(N - 1) ++ [N].

print_permutations([H | T]) ->
    print_list(H), print_permutations(T);
print_permutations([]) -> ok.

print_list([H | T]) ->
    io:format("~p ", [H]), print_list(T);
print_list([]) ->
    io:format("~n"), ok.

start() ->
    N = 2,
    P = permutations(signed_seq_list(N)),
    io:format("~w~n", [length(P)]),
    print_permutations(P).
