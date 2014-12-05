% Rosalind, nucleobase definitions.
% 12/04/2014.

-module(rosalind_util_nucleobase).
-export([nucleobase_list/1, nucleobase_list_print/1, nucleobase_list_string/1, nucleobase_list_length/1]).
-export([nucleobase_list_count_gc/1]).

% List of possible nucleobases.
-type nucleobase() :: adenine | guanine | thymine | cytosine | uracil.

% List of types exported.
-export_type([nucleobase/0]).

% Return length of nucleobase list.
nucleobase_list_length(L) -> length(L).

% Convert nucleobase string to a list.
nucleobase_list(L) -> nucleobase_list(L, []).
nucleobase_list([], L) -> L;
nucleobase_list([H | T], NL) ->
    case H of
        65 -> nucleobase_list(T, NL ++ [adenine]);
        67 -> nucleobase_list(T, NL ++ [cytosine]);
        71 -> nucleobase_list(T, NL ++ [guanine]);
        84 -> nucleobase_list(T, NL ++ [thymine]);
        85 -> nucleobase_list(T, NL ++ [uracil]);
        _ ->
            nucleobase_list(T, NL)
    end.

% Create a string representation of nucleobase string.
nucleobase_list_string([]) -> error;
nucleobase_list_string(N) -> nucleobase_list_string(N, []).

nucleobase_list_string([], S) -> S;
nucleobase_list_string([H | T], S) ->
    case H of
        adenine -> nucleobase_list_string(T, S ++ [65]);
        cytosine -> nucleobase_list_string(T, S ++ [67]);
        guanine -> nucleobase_list_string(T, S ++ [71]);
        thymine -> nucleobase_list_string(T, S ++ [84]);
        uracil -> nucleobase_list_string(T, S ++ [85])
    end.

% Print nucleobase sequence
nucleobase_list_print(L) ->
    io:format("~s~n", [nucleobase_list_string(L)]).

% Count number of GC nucleotides in a sequence.
nucleobase_list_count_gc(L) -> nucleobase_list_count_gc(L, 0).
nucleobase_list_count_gc([], Count) -> Count;
nucleobase_list_count_gc([H | T], Count) ->
    case H of
        cytosine -> nucleobase_list_count_gc(T, Count + 1);
        guanine -> nucleobase_list_count_gc(T, Count + 1);
        _ -> nucleobase_list_count_gc(T, Count)
    end.
