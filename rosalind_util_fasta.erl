% Rosalind, fasta definitions.
% 12/05/2014.

-module(rosalind_util_fasta).
-include("rosalind_util_fasta.hrl").
-import(rosalind_util_nucleobase, [nucleobase_list/1, nucleobase_list_string/1, nucleobase_list_print/1]).
-export([fasta_create/1, fasta_print_name/1, fasta_print/1]).

% Parse string and return a fasta record.
fasta_create([]) -> error;
fasta_create([H | T]) ->
    case H =/= 95 of
        true -> fasta_create(T);
        false -> fasta_create_helper(T, 1)
    end.

% Helper function to parse fasta code, not exported.
fasta_create_helper([], Code) -> error;
fasta_create_helper([H | T], Code) when (H > 47) and (H < 58) -> fasta_create_helper(T, Code * 10 + (H - 48));
fasta_create_helper(L, Code) -> #fasta{code=Code, sequence=nucleobase_list(L)}.

% Print fasta name.
fasta_print_name(#fasta{code=Code, sequence=Seq}) ->
    io:format("Rosalind_~w~n", [Code]).

% Print full fasta string.
fasta_print(F) ->
    #fasta{code=_Code, sequence=Seq} = F,
    io:format(">"),
    fasta_print_name(F),
    nucleobase_list_print(Seq).
