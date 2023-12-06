-module(road).
-export([main/0, main/1]).

main() ->
    File = "road.txt",
    {ok, BinaryContent} = file:read_file(File),
    optimal_path(parse_map(BinaryContent)).

%%% For run program outside the Erlang shell
%%% Ex(Compile): erlc read.erl 
%%% erl -noshell -run road main road.txt
main([FileName]) ->
    {ok, Bin} = file:read_file(FileName),
    Map = parse_map(Bin),
    io:format("~p~n", [optimal_path(Map)]),
    erlang:halt().

parse_map(Binary) when is_binary(Binary) ->
    parse_map(binary_to_list(Binary));
parse_map(Str) when is_list(Str) ->
    Tokens = string:tokens(Str, "\r\n\t"),
    Values = [list_to_integer(X) || X <- Tokens],
    group_values(Values, []).

group_values([], Acc) ->
    lists:reverse(Acc);
group_values([A,B,X|Rest], Acc) ->
    group_values(Rest, [{A, B, X}|Acc]).

shortest_step({A, B, X}, {{DistanceA, PathA}, {DistanceB, PathB}}) ->
    OptionA1 = {DistanceA + A, [{a, A}|PathA]},
    OptionA2 = {DistanceB + B + X, [{x, X}, {b, B}|PathB]},
    OptionB1 = {DistanceB + B, [{b, B}|PathB]},
    OptionB2 = {DistanceA + A + X, [{x, X}, {a, A}|PathA]},
    {erlang:min(OptionA1, OptionA2), erlang:min(OptionB1, OptionB2)}.

optimal_path(Map) ->
    {A, B} = lists:foldl(fun shortest_step/2, {{0,[]}, {0, []}}, Map),
    {_Distance, Path} = 
        if 
            hd(element(2,A)) =/= {x, 0} -> A;
            hd(element(2,B)) =/= {x, 0} -> B
        end,
    lists:reverse(Path).


%%% Manual Debug: shortest_step
% {50, 10, 30}
% A ={30, X30, B10}
% B ={10, B10}

% {5, 90, 20}
% A1 = {30 + 5, {5A, X30, B10}}
% A2 = {10 + 90 + 20, {20X, 90B, B10}}
% B1 = {10 + 90, {B90, B10}}
% B2 = {30 + 5 + 20 {20X, 5A, X30, B10}

% A1 = {35, {A5, X30, B10}}
% B2 = {55, {X20, A5, X30, B10}}

% {40, 2, 25}
% A1 = {35 + 40, {A40, A5, X30, B10}}
% A2 = {55 + 2, 25, {X25, B2, X20, A5, X30, B10}}
% B1 = {55 + 2 {B2, X20, A5, X30, B10}}
% B2 = {35 + 2 + 25 {X25, B2, A5, X30, B10}}

% A1 = {75, {A40, A5, X30, B10}}
% B1 = {57, {B2, X20, A5, X30, B10}}

% {10, 8, 0}
% A1 = {75 + 10, {A10, A40, A5, X30, B10}}
% A2 = {57 + 8 + 0, {X0, B8, B2, X20, A5, X30, B10}}
% B1 = {57 + 8, {B8, B2, X20, A5, X30, B10}}
% B2 = {75 + 10 + 0, {X0, A10, A40, A5, X30, B10}}