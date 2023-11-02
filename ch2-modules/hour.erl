-module(hour).
-export([get_hour_in_seconds/0, get_hour_in_miliseconds/0, get_debugmode_value/0]).
-define(HOUR, 3600). % in seconds
-ifdef(DEBUGMODE). % if the macro if defined regardless the value, macro DEBUG will be defined
-define(DEBUG(S), io:format("dbg: "++S)).
-else.
-define(DEBUG(S), ok).
-endif.
-compile([{d, 'DEBUGMODE'}]). % DEBUGMODE ON 
% -compile(). % DEBUGMODE OFF


get_hour_in_seconds() -> 
    ?DEBUG("3600 seconds"),
    ?HOUR.
get_hour_in_miliseconds() -> ?HOUR * 1000.

get_debugmode_value() -> ?DEBUG("IS DEBUG").