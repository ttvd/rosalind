% Rosalind, protein definitions.
% 12/06/2014.

-module(rosalind_util_protein).
-import(rosalind_util_nucleobase, [nucleobase_list/1]).
-export([protein_list_length/1, protein_list/1]).

% List of possible proteins (F, L, I, M, V, S, P, T, A, Y, H, Q, N, K, D, E, C, W, R, G, Stop).
-type protein() :: phenylalanine | leucine | isoleucine | methionine | valine | serine | proline | threonine |
                   alanine | tyrosine | histidine | glutamine | asparagine | lysine | aspartic_acid |
                   glutamic_acid | cysteine | arginine | glycine | protein_stop.

% List of types exported.
-export_type([protein/0]).

% Return length of protein list.
protein_list_length(L) -> length(L).

% Compute protein list from nucleobase list.
protein_list(NBL) -> protein_list(NBL, []).

protein_list([], PL) -> PL;
protein_list(NBL, PL) when length(NBL) < 3 -> error;

protein_list([uracil, uracil, uracil | T], PL) -> protein_list(T, PL ++ [phenylalanine]);
protein_list([uracil, uracil, cytosine | T], PL) -> protein_list(T, PL ++ [phenylalanine]);

protein_list([cytosine, uracil, uracil | T], PL) -> protein_list(T, PL ++ [leucine]);
protein_list([uracil, uracil, adenine | T], PL) -> protein_list(T, PL ++ [leucine]);
protein_list([cytosine, uracil, adenine | T], PL) -> protein_list(T, PL ++ [leucine]);
protein_list([cytosine, uracil, cytosine | T], PL) -> protein_list(T, PL ++ [leucine]);
protein_list([uracil, uracil, guanine | T], PL) -> protein_list(T, PL ++ [leucine]);
protein_list([cytosine, uracil, guanine | T], PL) -> protein_list(T, PL ++ [leucine]);

protein_list([adenine, uracil, uracil | T], PL) -> protein_list(T, PL ++ [isoleucine]);
protein_list([adenine, uracil, cytosine | T], PL) -> protein_list(T, PL ++ [isoleucine]);
protein_list([adenine, uracil, adenine | T], PL) -> protein_list(T, PL ++ [isoleucine]);

protein_list([adenine, uracil, guanine | T], PL) -> protein_list(T, PL ++ [methionine]);

protein_list([guanine, uracil, uracil | T], PL) -> protein_list(T, PL ++ [valine]);
protein_list([guanine, uracil, cytosine | T], PL) -> protein_list(T, PL ++ [valine]);
protein_list([guanine, uracil, adenine | T], PL) -> protein_list(T, PL ++ [valine]);
protein_list([guanine, uracil, guanine | T], PL) -> protein_list(T, PL ++ [valine]);

protein_list([uracil, cytosine, uracil | T], PL) -> protein_list(T, PL ++ [serine]);
protein_list([uracil, cytosine, cytosine | T], PL) -> protein_list(T, PL ++ [serine]);
protein_list([uracil, cytosine, adenine | T], PL) -> protein_list(T, PL ++ [serine]);
protein_list([uracil, cytosine, guanine | T], PL) -> protein_list(T, PL ++ [serine]);
protein_list([adenine, guanine, uracil | T], PL) -> protein_list(T, PL ++ [serine]);
protein_list([adenine, guanine, cytosine | T], PL) -> protein_list(T, PL ++ [serine]);

protein_list([guanine, cytosine, uracil | T], PL) -> protein_list(T, PL ++ [alanine]);
protein_list([guanine, cytosine, cytosine | T], PL) -> protein_list(T, PL ++ [alanine]);
protein_list([guanine, cytosine, adenine | T], PL) -> protein_list(T, PL ++ [alanine]);
protein_list([guanine, cytosine, guanine | T], PL) -> protein_list(T, PL ++ [alanine]);

protein_list([cytosine, cytosine, uracil | T], PL) -> protein_list(T, PL ++ [proline]);
protein_list([cytosine, cytosine, cytosine | T], PL) -> protein_list(T, PL ++ [proline]);
protein_list([cytosine, cytosine, adenine | T], PL) -> protein_list(T, PL ++ [proline]);
protein_list([cytosine, cytosine, guanine | T], PL) -> protein_list(T, PL ++ [proline]);

protein_list([adenine, cytosine, uracil | T], PL) -> protein_list(T, PL ++ [threonine]);
protein_list([adenine, cytosine, cytosine | T], PL) -> protein_list(T, PL ++ [threonine]);
protein_list([adenine, cytosine, adenine | T], PL) -> protein_list(T, PL ++ [threonine]);
protein_list([adenine, cytosine, guanine | T], PL) -> protein_list(T, PL ++ [threonine]);

protein_list([cytosine, guanine, uracil | T], PL) -> protein_list(T, PL ++ [argenine]);
protein_list([cytosine, guanine, cytosine | T], PL) -> protein_list(T, PL ++ [argenine]);
protein_list([cytosine, guanine, guanine | T], PL) -> protein_list(T, PL ++ [argenine]);
protein_list([adenine, guanine, guanine | T], PL) -> protein_list(T, PL ++ [argenine]);
protein_list([cytosine, guanine, adenine | T], PL) -> protein_list(T, PL ++ [argenine]);
protein_list([adenine, guanine, adenine | T], PL) -> protein_list(T, PL ++ [argenine]);

protein_list([uracil, adenine, uracil | T], PL) -> protein_list(T, PL ++ [tyrosine]);
protein_list([uracil, adenine, cytosine | T], PL) -> protein_list(T, PL ++ [tyrosine]);

protein_list([cytosine, adenine, uracil | T], PL) -> protein_list(T, PL ++ [histidine]);
protein_list([cytosine, adenine, cytosine | T], PL) -> protein_list(T, PL ++ [histidine]);

protein_list([adenine, adenine, uracil | T], PL) -> protein_list(T, PL ++ [asparagine]);
protein_list([adenine, adenine, cytosine | T], PL) -> protein_list(T, PL ++ [asparagine]);

protein_list([guanine, adenine, uracil | T], PL) -> protein_list(T, PL ++ [aspartic_acid]);
protein_list([guanine, adenine, cytosine | T], PL) -> protein_list(T, PL ++ [aspartic_acid]);

protein_list([uracil, adenine, adenine | T], PL) -> protein_list(T, PL ++ [protein_stop]);
protein_list([uracil, adenine, guanine | T], PL) -> protein_list(T, PL ++ [protein_stop]);
protein_list([uracil, guanine, adenine | T], PL) -> protein_list(T, PL ++ [protein_stop]);

protein_list([cytosine, adenine, adenine | T], PL) -> protein_list(T, PL ++ [glutamine]);
protein_list([cytosine, adenine, guanine | T], PL) -> protein_list(T, PL ++ [glutamine]);

protein_list([adenine, adenine, adenine | T], PL) -> protein_list(T, PL ++ [lysine]);
protein_list([adenine, adenine, guanine | T], PL) -> protein_list(T, PL ++ [lysine]);

protein_list([guanine, guanine, cytosine | T], PL) -> protein_list(T, PL ++ [glycine]);
protein_list([guanine, guanine, uracil | T], PL) -> protein_list(T, PL ++ [glycine]);
protein_list([guanine, guanine, adenine | T], PL) -> protein_list(T, PL ++ [glycine]);
protein_list([guanine, guanine, guanine | T], PL) -> protein_list(T, PL ++ [glycine]);

protein_list([guanine, adenine, adenine | T], PL) -> protein_list(T, PL ++ [glutamic_acid]);
protein_list([guanine, adenine, guanine | T], PL) -> protein_list(T, PL ++ [glutamic_acid]);

protein_list([uracil, guanine, uracil | T], PL) -> protein_list(T, PL ++ [cysteine]);
protein_list([uracil, guanine, cytosine | T], PL) -> protein_list(T, PL ++ [cysteine]);

protein_list([uracil, guanine, guanine | T], PL) -> protein_list(T, PL ++ [tryptophan]).
