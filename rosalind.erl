% Rosalind, common definitions.
% 12/01/2014.

-module(rosalind).
-export([nucleobase_list/1, nucleobase_list/2, nucleobase_list_print/1]).
-export([list_drop_head/2, list_print_space_separated/1]).

% List of possible nucleobases.
-type nucleobase() :: adenine | guanine | thymine | cytosine | uracil.

% List of types exported.
-export_type([nucleobase/0]).

% Convert nucleobase string to a list.
nucleobase_list(L) -> nucleobase_list(L, []).
nucleobase_list([], L) -> L;
nucleobase_list([H | T], NL) ->
    case H of
        65 -> nucleobase_list(T, NL ++ [adenine]);
        67 -> nucleobase_list(T, NL ++ [cytosine]);
        71 -> nucleobase_list(T, NL ++ [guanine]);
        84 -> nucleobase_list(T, NL ++ [thymine]);
        85 -> nucleobase_list(T, NL ++ [uracil])
    end.

% Print nucleobase sequence.
nucleobase_list_print([]) ->
    io:format("~n"), ok;
nucleobase_list_print([H | T]) ->
    case H of
        adenine -> io:format("A");
        cytosine -> io:format("C");
        guanine -> io:format("G");
        thymine -> io:format("T");
        uracil -> io:format("U")
    end,
    nucleobase_list_print(T).

% Drop N elements from beginning of a list.
list_drop_head([], _N) -> [];
list_drop_head(L, 0) -> L;
list_drop_head([_H | T], N) -> list_drop_head(T, N - 1).

% Print space separated elements of a list.
list_print_space_separated([]) -> ok;
list_print_space_separated([H | T]) -> io:format("~w ", [H]), list_print_space_separated(T).
