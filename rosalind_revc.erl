% Rosalind, problem REVC.
% 11/28/2014.

% AAAACCCGGT
% ACCGGGTTTT

% A 65 <-> T 84
% C 67 <-> G 71

-module(rosalind_revc).
-export([start/0]).

replace_revc([65 | T]) -> replace_revc(T) ++ [84];
replace_revc([84 | T]) -> replace_revc(T) ++ [65];
replace_revc([67 | T]) -> replace_revc(T) ++ [71];
replace_revc([71 | T]) -> replace_revc(T) ++ [67];
replace_revc([]) -> [].

start() -> io:format("~p", [replace_revc("AAAACCCGGT")]).
