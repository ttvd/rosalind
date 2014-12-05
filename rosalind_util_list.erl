% Rosalind, list definitions.
% 12/04/2014.

-module(rosalind_util_list).
-export([list_drop_head/2, list_print_space_separated/1]).

% Drop N elements from beginning of a list.
list_drop_head([], _N) -> [];
list_drop_head(L, 0) -> L;
list_drop_head([_H | T], N) -> list_drop_head(T, N - 1).

% Print space separated elements of a list.
list_print_space_separated([]) -> ok;
list_print_space_separated([H | T]) -> io:format("~w ", [H]), list_print_space_separated(T).
