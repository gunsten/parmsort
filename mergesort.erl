-module(mergesort).
-import(lists, [sort/1, split/2, seq/2]).
-compile(export_all).
%% eqc is only nescessary for testing
-include_lib("eqc/include/eqc.hrl").

-define(DEPTH, 4).

%% Sorts the list using the mergesort algorithm
msort([])  -> [];
msort([X]) -> [X];
msort(Xs)  ->
    {Ys, Zs} = in_half(Xs),
    merge(msort(Ys), msort(Zs)).

%% Splits a list in half
in_half(Xs) -> split(length(Xs) div 2, Xs).

%% Merge two lists
merge([], Ys) ->                        Ys;
merge(Xs, []) ->                        Xs;
merge([X|Xs], [Y|Ys]) when X =< Y ->    [X | merge(Xs, [Y|Ys])];
merge(Xs, [Y|Ys]) ->                    [Y | merge(Xs, Ys)].

%% Sorts the list using the mergesort algorithm,
%% forking at every depth of recursion
parmsort([])  -> [];
parmsort([X]) -> [X];
parmsort(Xs)  ->
    {Ys, Zs} = in_half(Xs),
    RPid = self(),
    Ref = make_ref(),
    spawn_link(fun() -> RPid ! {Ref, parmsort(Zs)} end),
    Ys_ = parmsort(Ys),
    receive {Ref, Zs_} -> merge(Ys_, Zs_) end.

%% Sorts the list using the mergesort algorithm,
%% forking until a certain depth of recursion
parmsort_depth(Xs) -> parmsort(Xs, ?DEPTH).

parmsort([], _)  -> [];
parmsort([X], _) -> [X];
parmsort(Xs, 0)  -> msort(Xs);
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

%% My shuffle is far too inefficient and takes way longer than
%% the actual sorting. The random access of elements is probably the culprit.

%% This elegant approach is borrowed from stack overflow:
%% https://stackoverflow.com/a/8820501
%%
%% Mark the elements of a sequence with random elements, and then sort
%% the resulting list on the marks.
shuffle(Xs) ->
    [X || {_,X} <- sort([ {rand:uniform(), X} || X <- Xs])].

benchmark() ->
                _ = rand:seed(exs1024s),
                [100, benchmark([msort, parmsort, parmsort_depth], 100),
                 1000, benchmark([msort, parmsort, parmsort_depth], 1000),
                 10000, benchmark([msort, parmsort, parmsort_depth], 10000),
                 100000, benchmark([msort, parmsort, parmsort_depth], 100000),
                 1000000, benchmark([msort, parmsort, parmsort_depth], 1000000)].

benchmark(Funs, Size) ->
    List = shuffle(seq(1, Size)),
    [single(Fun, List) || Fun <- Funs].

single(Fun, List) ->
    {Time, _} = timer:tc(?MODULE, Fun, [List]),
    {Fun, Time / 1000}. %millis
    
%% ---------------------------- TESTING ---------------------------------------

%% Property for testing that shuffle does not lose elements
prop_shuffle() -> ?FORALL(Xs, list(nat()), sort(Xs) == sort(shuffle(Xs))).

%% Property for testing merge
prop_merge() -> ?FORALL(Xs, list(nat()),
                    ?FORALL(Ys, list(nat()),
                        sort(Xs ++ Ys) == sort(merge(Xs, Ys)) )).

%% Property for testing msort
prop_msort() -> ?FORALL(Xs, list(nat()), sort(Xs) == msort(Xs)).

%% Properties for testing parmsort
prop_parmsort() -> ?FORALL(Xs, list(nat()), sort(Xs) == parmsort(Xs)).
prop_parmsort_depth() -> ?FORALL(Xs, list(nat()), sort(Xs) == parmsort(Xs, ?DEPTH)).
