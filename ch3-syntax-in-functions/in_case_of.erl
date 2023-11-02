-module(in_case_of).
-export([insert/2, beach/1]).

%%% If the if expression is like a guard, a case of expression is like the whole function head.
insert(X, []) ->
    [X];
insert(X, Set) -> 
    case lists:member(X, Set) of 
        true -> Set;
        false -> [X|Set]
    end.

beach(Temperature) ->
    case Temperature of 
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ -> 'avoid beach'
    end.        