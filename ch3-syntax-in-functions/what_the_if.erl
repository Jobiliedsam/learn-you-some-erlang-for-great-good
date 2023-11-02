-module(what_the_if).
-export([heh_fine/0, oh_god/1, help_me/1, avoid_true_branche/2]).

heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 -> % If whitout true branch
        fails  
    end.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
    true -> always_does % This is Erlang's if's else!
end.

help_me(Animal) ->
    Talk = if 
        Animal == cat -> "meow";
        Animal == beef -> "mooo";
        Animal == dog -> "bark";
        Animal == tree -> "bark";
        true -> "faefadfasdhfasjdf"
    end,
    {Animal, "says " ++ Talk ++ "!"}.
            
%%% True branches shoudl be avoided altogether.
avoid_true_branche(X, Y) ->
    if 
        X > Y -> true;
        X =< Y -> false
end.    
