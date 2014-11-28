% Rosalind, problem FIB.
% 11/28/2014.

% 5 3
% 19

-module(rosalind_fib).
-export([start/0]).

fib(1, _) -> 1;
fib(2, _) -> 1;
fib(X, Gen) -> fib(X - 1, Gen) + Gen * fib(X - 2, Gen).

start() -> io:format("~w", [fib(5, 3)]).
