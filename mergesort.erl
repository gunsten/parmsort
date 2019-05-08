-module(mergesort).
-include_lib("eqc/include/eqc.hrl").
-import(lists, [sort/1]).
-compile(export_all).

mergesort() -> ok.

merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) when X =< Y -> [X | merge(Xs, [Y|Ys])];
merge(Xs, [Y|Ys]) -> [Y | merge(Xs, Ys)].

prop_merge() -> ?FORALL(Xs, list(nat()),
                    ?FORALL(Ys, list(nat()),
                        sort(Xs ++ Ys) == sort(merge(Xs, Ys)) )).
