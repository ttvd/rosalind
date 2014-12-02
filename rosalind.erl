% Rosalind, common definitions.
% 12/01/2014.

-module(rosalind).
-export([nucleobase_list/1, nucleobase_list/2, nucleobase_list_print/1]).

% List of possible nucleobases.
-type nucleobase() :: adenine | guanine | thymine | cytosine | uracil.

% List of types exported.
-export_type([nucleobase/0]).

% Convert nucleobase string to a list.
nucleobase_list(L) -> nucleobase_list(L, []).
nucleobase_list([], L) -> L;
nucleobase_list([H | T], NL) ->
    case H of
        65 -> nucleobase_list(T, [adenine] ++ NL);
        67 -> nucleobase_list(T, [cytosine] ++ NL);
        71 -> nucleobase_list(T, [guanine] ++ NL);
        84 -> nucleobase_list(T, [thymine] ++ NL);
        85 -> nucleobase_list(T, [uracil] ++ NL)
    end.

nucleobase_list_print([]) ->
    io:format("~n"), ok;
nucleobase_list_print([H | T]) ->
    case H of
        65 -> io:format("A");
        67 -> io:format("C");
        71 -> io:format("G");
        84 -> io:format("T");
        85 -> io:format("U")
    end,
    nucleobase_list_print(T).
