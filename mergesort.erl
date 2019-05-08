-module(mergesort).
-compile(export_all).

mergesort() -> ok.

merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) when X =< Y -> [X | merge(Xs, [Y|Ys])];
merge(Xs, [Y|Ys]) -> [Y | merge(Xs, Ys)].
