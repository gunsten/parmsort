-module(mergesort).
-include_lib("eqc/include/eqc.hrl").
-import(lists, [sort/1, split/2]).
-compile(export_all).

%% Sorts the list using the mergesort algorithm
msort([]) -> [];
msort([X]) -> [X];
msort(Xs) ->
    {Ys, Zs} = in_half(Xs),
    merge(msort(Ys), msort(Zs)).

%% Splits a list in half
in_half(Xs) -> split(length(Xs) div 2, Xs).

%% Merge two lists
merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) when X =< Y -> [X | merge(Xs, [Y|Ys])];
merge(Xs, [Y|Ys]) -> [Y | merge(Xs, Ys)].

%% Sorts the list using the mergesort algorithm,
%% forking at every depth of recursion
parmsort([]) -> [];
parmsort([X]) -> [X];
parmsort(Xs) ->
    {Ys, Zs} = in_half(Xs),
    RPid = self(),
    Ref = make_ref(),
    spawn_link(fun() -> RPid ! {Ref, parmsort(Zs)} end),
    Ys_ = parmsort(Ys),
    receive {Ref, Zs_} -> merge(Ys_, Zs_) end.

%% Property for testing merge
prop_merge() -> ?FORALL(Xs, list(nat()),
                    ?FORALL(Ys, list(nat()),
                        sort(Xs ++ Ys) == sort(merge(Xs, Ys)) )).

%% Property for testing msort
prop_msort() -> ?FORALL(Xs, list(nat()), sort(Xs) == msort(Xs)).

%% Property for testing parmsort
prop_parmsort() -> ?FORALL(Xs, list(nat()), sort(Xs) == parmsort(Xs)).
