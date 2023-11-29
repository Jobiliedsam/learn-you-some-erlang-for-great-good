-module(calc).
-export([rpn/1, rpn_test/0]).

%%% Reverse Polish Notation = RPN
rpn(List) when is_list(List) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(List, " ")),
    Res.

rpn("+", [N1, N2|Stack]) -> [N2+N1|Stack];
rpn("-", [N1, N2|Stack]) -> [N2-N1|Stack];
rpn("*", [N1, N2|Stack]) -> [N2*N1|Stack];
rpn("/", [N1, N2|Stack]) -> [N2/N1|Stack];
rpn("^", [N1, N2|Stack]) -> [math:pow(N2, N1)|Stack];
rpn("ln", [N|Stack])     -> [math:log(N)|Stack];
rpn("sum", Stack)        -> [sum(Stack)];
rpn("sub", Stack)        -> [sub(lists:reverse(Stack))];
rpn("div", Stack)        -> [division(Stack)];
rpn("prod", Stack)       -> [prod(Stack)];
rpn("log10", [N|Stack])  -> [math:log10(N)|Stack];
rpn(Token, Stack) -> [read(Token)|Stack].

read(Number) ->
    case string:to_float(Number) of 
        {error, no_float} -> list_to_integer(Number);
        {Float, _} -> Float
    end.

sum(Numbers) ->
    lists:foldl(fun(Number, Acc) -> Number + Acc end, 0, Numbers).
sub([Head|Tail]) ->
    lists:foldl(fun(Number, Acc) -> Acc - Number end, Head, Tail).
division([Head|Tail]) ->
    lists:foldl(fun(Number, Acc) -> Acc / Number end, Head, Tail).
prod(Numbers) ->
    lists:foldl(fun(Number, Acc) -> Number * Acc end, 1, Numbers). 



rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try 
        rpn("90 34 12 +")
    catch 
        error:{badmatch, [_|_]} -> ok
    end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    3000 = rpn("1000 1000 2000 sum 1000 -"),
    196.0 = rpn("7 4 3 sum 2 ^"),
    56 = rpn("7 2 2 2 prod"),
    1 = rpn("1 1 1 1 prod"),
    0.25 = rpn("2 2 2 2 div"),
    -4 = rpn("2 2 4 sub"),
    -10 = rpn("2 2 4 6 sub"),
    ok.                