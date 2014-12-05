% Rosalind, problem SUBS.
% 12/01/2014.

% GATATATGCATATACTT
% ATAT
% 2 4 10

-module(rosalind_subs).
-export([start/0]).
-import(rosalind_util_list, [list_drop_head/2, list_print_space_separated/1]).

find_subs(MainStr, SubStr) -> find_subs(MainStr, MainStr, SubStr, SubStr, 0, 0, []).

find_subs([], MainStrOrig, Substr, SubstrOrig, MainStart, Pos, SubList) -> SubList;
find_subs(MainStr, MainStrOrig, [], SubstrOrig, MainStart, Pos, SubList) ->
    Index = Pos + MainStart + 1 - length(SubstrOrig),
    find_subs(list_drop_head(MainStrOrig, MainStart + 1), MainStrOrig, SubstrOrig, SubstrOrig, MainStart + 1, 0, SubList ++ [Index]);
find_subs([H | MainStrT], MainStrOrig, [H | SubstrT], SubstrOrig, MainStart, Pos, SubList) ->
    find_subs(MainStrT, MainStrOrig, SubstrT, SubstrOrig, MainStart, Pos + 1, SubList);
find_subs([_H1 | MainStrT], MainStrOrig, [_H2 | SubstrT], SubstrOrig, MainStart, Pos, SubList) ->
    find_subs(list_drop_head(MainStrOrig, MainStart + 1), MainStrOrig, SubstrOrig, SubstrOrig, MainStart + 1, 0, SubList).

start() ->
    M = "GATATATGCATATACTT",
    S = "ATAT",
    L = find_subs(M, S),
    list_print_space_separated(L).
