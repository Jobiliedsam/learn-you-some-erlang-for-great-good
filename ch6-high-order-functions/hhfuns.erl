-module(hhfuns).
-export([one/0, two/0, add/2, increment/1, decrement/1, map/2, incr/1, decr/1, even/1, old_men/1, filter/2, max/1, min/1, sum/2, fold/2, fold/3]).
-compile({no_auto_import,[max/2, min/2]}).
one() -> 1.
two() -> 2.

%%% Pass functions from outside a module: fun Module:Function/Arity
%%% Consume example: hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
add(X, Y) -> X() + Y().


increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

%%% Pattern: Apply a function on each element of an list
%%% Caller map with anonymous function: hhfuns:map(fun(N) -> N + 1 end, List).
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

%%% Pattern Filters:
%%% Only keep even numbers.
even(List) -> lists:reverse(even(List,[])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> 
    even(T, [H|Acc]);
even([_|T], Acc) ->
    even(T, Acc).


%%% Only keep men older than 60
old_men(List) -> lists:reverse(old_men(List, [])).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
    old_men(People, Acc).

%%% Pattern: evaluate a predicate to each elements of a list, and returns that elements succeed on predicate test.
%%% Predicate example: OldMen = fun({Gender, Age}) -> Gender == male andalso Age > 60 end.
%%% Predicate example: Even = fun(N) -> N rem 2 == 0 end.
filter(Predicate, List) -> lists:reverse(filter(Predicate, List, [])).

filter(_, [], Acc) -> Acc;
filter(Predicate, [H|T], Acc) ->
    case Predicate(H) of 
        true -> filter(Predicate, T, [H|Acc]);
        false -> filter(Predicate, T, Acc)
    end.

%%% Pattern Fold
max([H|T]) -> max(T, H).

max([], Max) -> Max;
max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max) -> max(T, Max).

min([H|T]) -> min(T, H).

min([], Min) -> Min;
min([H|T], Min) when H < Min ->
    min(T, H);
min([_|T], Min) ->
    min(T, Min).

sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, Sum + H).

%%% Pattern: Apply some function to each element of a list successively to reduce the elements to a single value

%%% fold/2 that assumes start value equal head of list
fold(Function, [H|T]) -> fold(Function, H, T).

fold(_, Start, []) -> Start;
fold(Function, Start, [H|T]) -> fold(Function, Function(H,Start), T).
