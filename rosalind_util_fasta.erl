% Rosalind, fasta definitions.
% 12/05/2014.

-module(rosalind_util_fasta).
-include("rosalind_util_fasta.hrl").
-import(rosalind_util_nucleobase, [nucleobase_list/1, nucleobase_list_string/1, nucleobase_list_print/1]).
-import(rosalind_util_nucleobase, [nucleobase_list_length/1, nucleobase_list_count_gc/1]).

-export([fasta_create/1, fasta_print_name/1, fasta_print/1, fasta_gc_content/1, fasta_parse_strings/1]).

% Parse multiple fasta strings into a list. We treat > as separator.
fasta_parse_strings([]) -> error;
fasta_parse_strings([$> | T]) -> fasta_parse_strings_collect(T, [], []).
fasta_parse_strings_collect([$> | T], Buf, FastaList) -> fasta_parse_strings_collect(T, [], FastaList ++ [fasta_create(Buf)]);
fasta_parse_strings_collect([H | T], Buf, FastaList) -> fasta_parse_strings_collect(T, Buf ++ [H], FastaList);
fasta_parse_strings_collect([], Buf, FastaList) -> FastaList ++ [fasta_create(Buf)].

% Parse string and return a fasta record.
fasta_create([]) -> error;
fasta_create([$_ | T]) -> fasta_create_helper(T, 0);
fasta_create([_H | T]) -> fasta_create(T).

% Helper function to parse fasta code, not exported.
fasta_create_helper([], _Code) -> error;
fasta_create_helper([H | T], Code) when (H >= $0) and (H =< $9) -> fasta_create_helper(T, Code * 10 + (H - 48));
fasta_create_helper(L, Code) -> #fasta{code=Code, sequence=nucleobase_list(L)}.

% Print fasta name.
fasta_print_name(#fasta{code=Code, sequence=_Seq}) ->
    io:format("Rosalind_~w~n", [Code]).

% Print full fasta string.
fasta_print(F) ->
    #fasta{code=_Code, sequence=Seq} = F,
    io:format(">"),
    fasta_print_name(F),
    nucleobase_list_print(Seq).

% Get GC content of fasta.
fasta_gc_content(#fasta{code=_Code, sequence=Seq}) ->
    100.0 * (nucleobase_list_count_gc(Seq) / nucleobase_list_length(Seq)).
