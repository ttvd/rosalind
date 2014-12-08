% Rosalind, protein definitions.
% 12/06/2014.

-module(rosalind_util_protein).
-import(rosalind_util_nucleobase, [nucleobase_list/1]).
-export([protein_list_length/1, protein_list_string/1, protein_list_print/1, protein_list_mass/1]).
-export([protein_list/1, protein_list_from_nucleobase/1]).

% List of possible proteins (F, L, I, M, V, S, P, T, A, Y, H, Q, N, K, D, E, C, W, R, G, Stop).
-type protein() :: phenylalanine | leucine | isoleucine | methionine | valine | serine | proline | threonine |
                   alanine | tyrosine | histidine | glutamine | asparagine | lysine | aspartic_acid |
                   glutamic_acid | cysteine | arginine | glycine | tryptophan | protein_stop.

% List of types exported.
-export_type([protein/0]).

% Return length of protein list.
protein_list_length(L) -> length(L).

% Compute protein list from string.
protein_list(PS) -> protein_list(PS, []).
protein_list([], S) -> S;
protein_list([$F | T], S) -> protein_list(T, S ++ [phenylalanine]);
protein_list([$L | T], S) -> protein_list(T, S ++ [leucine]);
protein_list([$I | T], S) -> protein_list(T, S ++ [isoleucine]);
protein_list([$M | T], S) -> protein_list(T, S ++ [methionine]);
protein_list([$V | T], S) -> protein_list(T, S ++ [valine]);
protein_list([$S | T], S) -> protein_list(T, S ++ [serine]);
protein_list([$P | T], S) -> protein_list(T, S ++ [proline]);
protein_list([$T | T], S) -> protein_list(T, S ++ [threonine]);
protein_list([$A | T], S) -> protein_list(T, S ++ [alanine]);
protein_list([$Y | T], S) -> protein_list(T, S ++ [tyrosine]);
protein_list([$H | T], S) -> protein_list(T, S ++ [histidine]);
protein_list([$Q | T], S) -> protein_list(T, S ++ [glutamine]);
protein_list([$N | T], S) -> protein_list(T, S ++ [asparagine]);
protein_list([$K | T], S) -> protein_list(T, S ++ [lysine]);
protein_list([$D | T], S) -> protein_list(T, S ++ [aspartic_acid]);
protein_list([$E | T], S) -> protein_list(T, S ++ [glutamic_acid]);
protein_list([$C | T], S) -> protein_list(T, S ++ [cysteine]);
protein_list([$R | T], S) -> protein_list(T, S ++ [argenine]);
protein_list([$G | T], S) -> protein_list(T, S ++ [glycine]);
protein_list([$W | T], S) -> protein_list(T, S ++ [tryptophan]).

% Compute protein list from nucleobase list.
protein_list_from_nucleobase(NBL) -> protein_list_from_nucleobase(NBL, []).

protein_list_from_nucleobase([], PL) -> PL;
protein_list_from_nucleobase(NBL, PL) when length(NBL) < 3 -> error;

protein_list_from_nucleobase([uracil, uracil, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [phenylalanine]);
protein_list_from_nucleobase([uracil, uracil, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [phenylalanine]);

protein_list_from_nucleobase([cytosine, uracil, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);
protein_list_from_nucleobase([uracil, uracil, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);
protein_list_from_nucleobase([cytosine, uracil, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);
protein_list_from_nucleobase([cytosine, uracil, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);
protein_list_from_nucleobase([uracil, uracil, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);
protein_list_from_nucleobase([cytosine, uracil, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [leucine]);

protein_list_from_nucleobase([adenine, uracil, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [isoleucine]);
protein_list_from_nucleobase([adenine, uracil, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [isoleucine]);
protein_list_from_nucleobase([adenine, uracil, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [isoleucine]);

protein_list_from_nucleobase([adenine, uracil, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [methionine]);

protein_list_from_nucleobase([guanine, uracil, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [valine]);
protein_list_from_nucleobase([guanine, uracil, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [valine]);
protein_list_from_nucleobase([guanine, uracil, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [valine]);
protein_list_from_nucleobase([guanine, uracil, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [valine]);

protein_list_from_nucleobase([uracil, cytosine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);
protein_list_from_nucleobase([uracil, cytosine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);
protein_list_from_nucleobase([uracil, cytosine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);
protein_list_from_nucleobase([uracil, cytosine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);
protein_list_from_nucleobase([adenine, guanine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);
protein_list_from_nucleobase([adenine, guanine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [serine]);

protein_list_from_nucleobase([guanine, cytosine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [alanine]);
protein_list_from_nucleobase([guanine, cytosine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [alanine]);
protein_list_from_nucleobase([guanine, cytosine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [alanine]);
protein_list_from_nucleobase([guanine, cytosine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [alanine]);

protein_list_from_nucleobase([cytosine, cytosine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [proline]);
protein_list_from_nucleobase([cytosine, cytosine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [proline]);
protein_list_from_nucleobase([cytosine, cytosine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [proline]);
protein_list_from_nucleobase([cytosine, cytosine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [proline]);

protein_list_from_nucleobase([adenine, cytosine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [threonine]);
protein_list_from_nucleobase([adenine, cytosine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [threonine]);
protein_list_from_nucleobase([adenine, cytosine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [threonine]);
protein_list_from_nucleobase([adenine, cytosine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [threonine]);

protein_list_from_nucleobase([cytosine, guanine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);
protein_list_from_nucleobase([cytosine, guanine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);
protein_list_from_nucleobase([cytosine, guanine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);
protein_list_from_nucleobase([adenine, guanine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);
protein_list_from_nucleobase([cytosine, guanine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);
protein_list_from_nucleobase([adenine, guanine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [argenine]);

protein_list_from_nucleobase([uracil, adenine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [tyrosine]);
protein_list_from_nucleobase([uracil, adenine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [tyrosine]);

protein_list_from_nucleobase([cytosine, adenine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [histidine]);
protein_list_from_nucleobase([cytosine, adenine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [histidine]);

protein_list_from_nucleobase([adenine, adenine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [asparagine]);
protein_list_from_nucleobase([adenine, adenine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [asparagine]);

protein_list_from_nucleobase([guanine, adenine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [aspartic_acid]);
protein_list_from_nucleobase([guanine, adenine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [aspartic_acid]);

protein_list_from_nucleobase([uracil, adenine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [protein_stop]);
protein_list_from_nucleobase([uracil, adenine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [protein_stop]);
protein_list_from_nucleobase([uracil, guanine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [protein_stop]);

protein_list_from_nucleobase([cytosine, adenine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glutamine]);
protein_list_from_nucleobase([cytosine, adenine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glutamine]);

protein_list_from_nucleobase([adenine, adenine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [lysine]);
protein_list_from_nucleobase([adenine, adenine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [lysine]);

protein_list_from_nucleobase([guanine, guanine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glycine]);
protein_list_from_nucleobase([guanine, guanine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glycine]);
protein_list_from_nucleobase([guanine, guanine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glycine]);
protein_list_from_nucleobase([guanine, guanine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glycine]);

protein_list_from_nucleobase([guanine, adenine, adenine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glutamic_acid]);
protein_list_from_nucleobase([guanine, adenine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [glutamic_acid]);

protein_list_from_nucleobase([uracil, guanine, uracil | T], PL) -> protein_list_from_nucleobase(T, PL ++ [cysteine]);
protein_list_from_nucleobase([uracil, guanine, cytosine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [cysteine]);

protein_list_from_nucleobase([uracil, guanine, guanine | T], PL) -> protein_list_from_nucleobase(T, PL ++ [tryptophan]).

% Create string representation of a protein list.
protein_list_string(PL) -> protein_list_string(PL, []).

protein_list_string([], S) -> S;
protein_list_string([phenylalanine | T], S) -> protein_list_string(T, S ++ "F");
protein_list_string([leucine | T], S) -> protein_list_string(T, S ++ "L");
protein_list_string([isoleucine | T], S) -> protein_list_string(T, S ++ "I");
protein_list_string([methionine | T], S) -> protein_list_string(T, S ++ "M");
protein_list_string([valine | T], S) -> protein_list_string(T, S ++ "V");
protein_list_string([serine | T], S) -> protein_list_string(T, S ++ "S");
protein_list_string([proline | T], S) -> protein_list_string(T, S ++ "P");
protein_list_string([threonine | T], S) -> protein_list_string(T, S ++ "T");
protein_list_string([alanine | T], S) -> protein_list_string(T, S ++ "A");
protein_list_string([tyrosine | T], S) -> protein_list_string(T, S ++ "Y");
protein_list_string([histidine | T], S) -> protein_list_string(T, S ++ "H");
protein_list_string([glutamine | T], S) -> protein_list_string(T, S ++ "Q");
protein_list_string([asparagine | T], S) -> protein_list_string(T, S ++ "N");
protein_list_string([lysine | T], S) -> protein_list_string(T, S ++ "K");
protein_list_string([aspartic_acid | T], S) -> protein_list_string(T, S ++ "D");
protein_list_string([glutamic_acid | T], S) -> protein_list_string(T, S ++ "E");
protein_list_string([cysteine | T], S) -> protein_list_string(T, S ++ "C");
protein_list_string([argenine | T], S) -> protein_list_string(T, S ++ "R");
protein_list_string([glycine | T], S) -> protein_list_string(T, S ++ "G");
protein_list_string([tryptophan | T], S) -> protein_list_string(T, S ++ "W");
protein_list_string([protein_stop | T], S) -> protein_list_string(T, S).

% Compute mass of protein sequence.
protein_list_mass(PL) -> protein_list_mass(PL, 0.0).

protein_list_mass([], Mass) -> Mass;
protein_list_mass([phenylalanine | T], Mass) -> protein_list_mass(T, Mass + 147.06841);
protein_list_mass([leucine | T], Mass) -> protein_list_mass(T, Mass + 113.08406);
protein_list_mass([isoleucine | T], Mass) -> protein_list_mass(T, Mass + 113.08406);
protein_list_mass([methionine | T], Mass) -> protein_list_mass(T, Mass + 131.04049);
protein_list_mass([valine | T], Mass) -> protein_list_mass(T, Mass + 99.06841);
protein_list_mass([serine | T], Mass) -> protein_list_mass(T, Mass + 87.03203);
protein_list_mass([proline | T], Mass) -> protein_list_mass(T, Mass + 97.05276);
protein_list_mass([threonine | T], Mass) -> protein_list_mass(T, Mass + 101.04768);
protein_list_mass([alanine | T], Mass) -> protein_list_mass(T, Mass + 71.03711);
protein_list_mass([tyrosine | T], Mass) -> protein_list_mass(T, Mass + 163.06333);
protein_list_mass([histidine | T], Mass) -> protein_list_mass(T, Mass + 137.05891);
protein_list_mass([glutamine | T], Mass) -> protein_list_mass(T, Mass + 128.05858);
protein_list_mass([asparagine | T], Mass) -> protein_list_mass(T, Mass + 114.04293);
protein_list_mass([lysine | T], Mass) -> protein_list_mass(T, Mass + 128.09496);
protein_list_mass([aspartic_acid | T], Mass) -> protein_list_mass(T, Mass + 115.02694);
protein_list_mass([glutamic_acid | T], Mass) -> protein_list_mass(T, Mass + 129.04259);
protein_list_mass([cysteine | T], Mass) -> protein_list_mass(T, Mass + 103.00919);
protein_list_mass([argenine | T], Mass) -> protein_list_mass(T, Mass + 156.10111);
protein_list_mass([glycine | T], Mass) -> protein_list_mass(T, Mass + 57.02146);
protein_list_mass([tryptophan | T], Mass) -> protein_list_mass(T, Mass + 186.07931);
protein_list_mass([protein_stop | T], Mass) -> Mass.

% Print protein sequence.
protein_list_print(PL) ->
    io:format("~s~n", [protein_list_string(PL)]).
