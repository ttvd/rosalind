% Rosalind, list definitions.
% 12/04/2014.

-module(rosalind_util_list).
-export([list_drop_head/2, list_print_space_separated/1, list_is_palindrome/1, lists_compare/2, lists_reverse/1]).

% Drop N elements from beginning of a list.
list_drop_head([], _N) -> [];
list_drop_head(L, 0) -> L;
list_drop_head([_H | T], N) -> list_drop_head(T, N - 1).

% Print space separated elements of a list.
list_print_space_separated([]) -> ok;
list_print_space_separated([H | T]) -> io:format("~w ", [H]), list_print_space_separated(T).

% Return true if list is a palindrome.
list_is_palindrome([]) -> true;
list_is_palindrome(L) when length(L) =:= 1 -> true;
list_is_palindrome(L) ->
    [H | T] = L,
    case lists:last(L) =:= H of
        true -> list_is_palindrome(lists:droplast(T));
        false -> false
    end.

% Compare two lists element wise.
lists_compare([], []) -> true;
lists_compare([], _R) -> false;
lists_compare(_L, []) -> false;
lists_compare([H | TL], [H | TR]) -> lists_compare(TL, TR);
lists_compare([_HL | _TL], [_HR | _TR]) -> false.

% Reverse a list.
lists_reverse([]) -> [];
lists_reverse([H | T]) -> lists_reverse(T) ++ [H].
