% Rosalind, problem GC.
% 12/05/2014.

-module(rosalind_gc).
-include("rosalind_util_fasta.hrl").
-export([start/0]).
-import(rosalind_util_fasta, [fasta_create/1, fasta_print_name/1, fasta_gc_content/1, fasta_parse_strings/1]).

% Find fasta with largest GC content.
locate_highest_gc_fasta(FL) ->
    [H | T] = FL,
    locate_highest_gc_fasta(T, H, fasta_gc_content(H)).
locate_highest_gc_fasta([], Fasta, CurrentGC) -> Fasta;
locate_highest_gc_fasta([H | T], Fasta, CurrentGC) ->
    GC = fasta_gc_content(H),
    case GC > CurrentGC of
        true -> locate_highest_gc_fasta(T, H, GC);
        false -> locate_highest_gc_fasta(T, Fasta, CurrentGC)
    end.

start() ->
    S = ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT",
    FL = fasta_parse_strings(S),
    F = locate_highest_gc_fasta(FL),
    fasta_print_name(F),
    io:format("~w~n", [fasta_gc_content(F)]).
