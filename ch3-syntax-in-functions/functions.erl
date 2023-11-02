-module(functions).
-compile(export_all).

%%% Use pattern matching to define which parts of a function should be used and bind the values we need at the same time.
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

%%% Pattern matching in functions with the cons operator (|) and the "don't care" or discard variable (_).
head([H|_]) -> H.

tail([_|T]) -> T.

second([_, X|_]) -> X.

%%% Use pattern matching and functions to compare and know if two parameters passed to a function are the same
same(X, X) ->
    true;
same(_,_) ->
    false.

%%% Advanced Example
valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n", [Date,Y,M,D]),
    io:format("The Time tuple (~p) indicates: ~p/~p/~p,~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data! ~n").


%%% Guards are additional clauses that can go in a function's head to make pattern matching more expressive.
%%% Use Guards with pattern matching
old_enough(X) when X >= 16 ->
    true;
old_enough(_) ->
    false.

%%% In guard expressions the comma (,) acts in similar manner to the operator andalso and the semicolon (;) acts a bit like orelse.
right_age(X) when X >= 16, X =< 104 -> % Andalso
    true;
right_age(_) ->
    false.

wrong_age(X) when X < 16; X > 104 ->
    true;
wrong_age(_) ->
    false.

%%% An if clause acts like a guard and shares the guard syntax, but outside a function clause's head. 
%%% In fact, if clauses are called guard patterns.