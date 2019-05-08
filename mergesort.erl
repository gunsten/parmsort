-module(mergesort).
-include_lib("eqc/include/eqc.hrl").
-import(lists, [sort/1, split/2]).
-compile(export_all).

%% Sorts the list using the mergesort algorithm
msort([]) -> [];
msort([X]) -> [X];
msort(Xs) ->
    {Ys, Zs} = split(length(Xs) div 2, Xs),
    merge(msort(Ys), msort(Zs)).

%% Merge two lists
merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) when X =< Y -> [X | merge(Xs, [Y|Ys])];
merge(Xs, [Y|Ys]) -> [Y | merge(Xs, Ys)].

%% Property for testing merge
prop_merge() -> ?FORALL(Xs, list(nat()),
                    ?FORALL(Ys, list(nat()),
                        sort(Xs ++ Ys) == sort(merge(Xs, Ys)) )).

%% Property for testing msort
prop_msort() -> ?FORALL(Xs, list(nat()), sort(Xs) == msort(Xs)).
