-module(recursive).

-export([fac/1, tail_fact/1, len/1, tail_len/1, duplicate/2, tail_duplicate/2, reverse/1,
         tail_reverse/1, sublist/2, tail_sublist/2, zip/2, tail_zip/2, lenient_zip/2,
         tail_lenient_zip/2]).

%%% fac/1 return factorial of a number (N!)
fac(N) when N == 0 ->
    1;
fac(N) when N > 0 ->
    N * fac(N - 1).

%%% tail_fact/1 is a tail recursive functio version of fac
tail_fact(N) ->
    tail_fact(N, 1). % Exported("Public") function

tail_fact(N, Accumulator) when N == 0 ->
    Accumulator; % Not exported ("Private") function
tail_fact(N, Accumulator) when N > 0 ->
    tail_fact(N - 1, Accumulator * N).

%%% length/1 count how many elements a list contains.
len([]) ->
    0;
len([_ | T]) ->
    1 + len(T).

tail_len(List) ->
    tail_len(List, 0).

tail_len([], Accumulator) ->
    Accumulator;
tail_len([_ | T], Accumulator) ->
    tail_len(T, Accumulator + 1).

%%% duplicate/2 this function takes an integer as its first parameter and any other term as its second parameter.
%%% It then creates a list of as many copies of the term as specified by the integer
duplicate(0, _) ->
    [];
duplicate(N, Term) when N > 0 ->
    [Term | duplicate(N - 1, Term)].

tail_duplicate(N, Term) ->
    tail_duplicate(N, Term, []).

tail_duplicate(0, _, Duplicates) ->
    Duplicates;
tail_duplicate(N, Term, Duplicates) when N > 0 ->
    tail_duplicate(N - 1, Term, [Term | Duplicates]).

%%% reverse/1, this function take a list and reverse order
reverse([]) ->
    [];
reverse([H | T]) ->
    reverse(T) ++ [H].

tail_reverse(List) ->
    tail_reverse(List, []).

tail_reverse([], ReversedList) ->
    ReversedList;
tail_reverse([H | T], ReversedList) ->
    tail_reverse(T, [H | ReversedList]).

%%% sublist/2 this function takes a list and an integer, and returns the N first elements of the list
sublist(_, 0) ->
    [];
sublist([], _) ->
    [];
sublist([H | T], N) when N > 0 ->
    [H | sublist(T, N - 1)].

tail_sublist(List, N) ->
    tail_reverse(tail_sublist(List, N, [])). % tail_sublist/3 return elements of a list in inverse order

tail_sublist(_, 0, SubList) ->
    SubList;
tail_sublist([], _, SubList) ->
    SubList;
tail_sublist([H | T], N, SubList) when N > 0 ->
    tail_sublist(T, N - 1, [H | SubList]).

%%% zip/2, this function takes two lists of the same length as parameters and joins them as a list of tuples
zip([], []) ->
    [];
zip([Hx | Tx], [Hy | Ty]) ->
    [{Hx, Hy} | zip(Tx, Ty)].

tail_zip(FirstList, SecondList) ->
    tail_reverse(tail_zip(FirstList, SecondList, [])).

tail_zip([], [], ZippedList) ->
    ZippedList;
tail_zip([Hf | Tf], [Hs | Ts], ZippedList) ->
    tail_zip(Tf, Ts, [{Hf, Hs} | ZippedList]).

%%% lenient_zip/2, this function is a lenient version of zip, that acepts list of the diferent length
lenient_zip([], _) ->
    [];
lenient_zip(_, []) ->
    [];
lenient_zip([Hx | Tx], [Hy | Ty]) ->
    [{Hx, Hy} | lenient_zip(Tx, Ty)].

tail_lenient_zip(FirstList, SecondList) ->
    tail_reverse(tail_lenient_zip(FirstList, SecondList, [])).

tail_lenient_zip([], _, ZippedList) ->
    ZippedList;
tail_lenient_zip(_, [], ZippedList) ->
    ZippedList;
tail_lenient_zip([Hf | Tf], [Hs | Ts], ZippedList) ->
    tail_lenient_zip(Tf, Ts, [{Hf, Hs} | ZippedList]).
