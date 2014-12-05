% Rosalind, problem REVC.
% 11/28/2014.

% AAAACCCGGT
% ACCGGGTTTT

% A 65 <-> T 84
% C 67 <-> G 71

-module(rosalind_revc).
-export([start/0]).
-import(rosalind_util_nucleobase, [nucleobase_list/1, nucleobase_list_print/1]).

replace_revc([adenine | T]) -> replace_revc(T) ++ [thymine];
replace_revc([thymine | T]) -> replace_revc(T) ++ [adenine];
replace_revc([cytosine | T]) -> replace_revc(T) ++ [guanine];
replace_revc([guanine | T]) -> replace_revc(T) ++ [cytosine];
replace_revc([]) -> [].

start() ->
    L = nucleobase_list("AAAACCCGGT"),
    nucleobase_list_print(replace_revc(L)).
