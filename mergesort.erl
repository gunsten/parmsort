-module(mergesort).
-include_lib("eqc/include/eqc.hrl").
-import(lists, [sort/1, split/2, seq/2]).
-compile(export_all).

-define(DEPTH, 4).

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

%% Sorts the list using the mergesort algorithm,
%% forking until a certain depth of recursion
parmsort_depth(Xs) -> parmsort(Xs, ?DEPTH).

parmsort([], _) -> [];
parmsort([X], _) -> [X];
parmsort(Xs, 0) -> msort(Xs);
parmsort(Xs, Depth) ->
    {Ys, Zs} = in_half(Xs),
    RPid = self(),
    Ref = make_ref(),
    spawn_link(fun() -> RPid ! {Ref, parmsort(Zs, Depth-1)} end),
    Ys_ = parmsort(Ys, Depth-1),
    receive {Ref, Zs_} -> merge(Ys_, Zs_) end.

%% ---------------------------- BENCHMARKING ----------------------------------

%% Shuffles a list
%shuffle([]) -> [];
%shuffle(Xs) ->
%    N = rand:uniform(length(Xs)) - 1,
%    {First, [X|Last]} = split(N, Xs),
%    [X | shuffle(First ++ Last)].

%% My shuffle is far too inefficient - this is borrowed from stack overflow:
%% https://stackoverflow.com/a/8820501
shuffle(Xs) ->
    [X || {_,X} <- sort([ {random:uniform(), X} || X <- Xs])].

%% Property for testing that shuffle does not lose elements
prop_shuffle() -> ?FORALL(Xs, list(nat()), sort(Xs) == sort(shuffle(Xs))).

benchmark() -> benchmark([msort, parmsort, parmsort_depth], 100000).

benchmark(Funs, Size) ->
    List = shuffle(seq(1, Size)),
    [single(Fun, List) || Fun <- Funs].

single(Fun, List) ->
    {Time, _} = timer:tc(?MODULE, Fun, [List]),
    {Fun, Time}.
    
%% ---------------------------- TESTING ---------------------------------------

%% Property for testing merge
prop_merge() -> ?FORALL(Xs, list(nat()),
                    ?FORALL(Ys, list(nat()),
                        sort(Xs ++ Ys) == sort(merge(Xs, Ys)) )).

%% Property for testing msort
prop_msort() -> ?FORALL(Xs, list(nat()), sort(Xs) == msort(Xs)).

%% Properties for testing parmsort
prop_parmsort() -> ?FORALL(Xs, list(nat()), sort(Xs) == parmsort(Xs)).
prop_parmsort_depth() -> ?FORALL(Xs, list(nat()), sort(Xs) == parmsort(Xs, ?DEPTH)).
