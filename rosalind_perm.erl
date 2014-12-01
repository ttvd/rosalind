% Rosalind, problem PERM.
% 11/30/2014.

% 3
% 6
% List of all permutations in any order.

-module(rosalind_perm).
-export([start/0]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

permutations([]) -> [[]];
permutations(L)  -> [[H | T] || H <- L, T <- permutations (L -- [H])].

print_permutations([H | T]) ->
    print_list(H), print_permutations(T);
print_permutations([]) -> ok.

print_list([H | T]) ->
    io:format("~p ", [H]), print_list(T);
print_list([]) ->
    io:format("~n"), ok.

start() ->
    N = 3,
    io:format("~w~n", [factorial(N)]),
    Perms = permutations(lists:seq(1, N)),
    print_permutations(Perms).
