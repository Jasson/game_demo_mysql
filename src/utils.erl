%%---------------------------------------------------------------------
%% File     :utils
%% Autho    :langxianzhe@163.com
%% Description: public fuction
%% Created : 2013-09-02
%%----------------------------------------------------------------------
-module(utils).

-export([get_datetime/0,
         get_datetime/1,
         get_id/0,
         datetime_to_seconds/1]).

-include("game.hrl").

get_id() ->
    {T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.10B~6.10.0B~6.10.0B", [T1,T2,T3])). 

get_datetime() ->
    {{Y,M,D},{H,Mn,S}}=erlang:localtime(),
    F=fun(X) -> if X>9  -> integer_to_list(X) ; true -> "0"++integer_to_list(X) end end,
    Ys = F(Y),
    Ms = F(M), 
    Ds = F(D), 
    Hs = F(H),
    Mns = F(Mn), 
    Ss = F(S),
    Date = Ys ++ "-" ++ Ms ++ "-" ++ Ds ++ " " ++ Hs ++ ":" ++ Mns ++":" ++Ss,
    Date.

get_datetime(Datetime) ->
    {{Y,M,D},{H,Mn,S}}=Datetime,
    F=fun(X) -> if X>9  -> integer_to_list(X) ; true -> "0"++integer_to_list(X) end end,
    Ys = F(Y),
    Ms = F(M),
    Ds = F(D),
    Hs = F(H),
    Mns = F(Mn),
    Ss = F(S),
    Date = Ys ++ "-" ++ Ms ++ "-" ++ Ds ++ " " ++ Hs ++ ":" ++ Mns ++":" ++Ss,
    Date.
datetime_to_seconds(Datetime) when is_list(Datetime) ->
    [YY,MM,DD,H,M,S] = string:tokens(Datetime,"- :"),
    Day = {list_to_integer(YY),list_to_integer(MM),list_to_integer(DD)},
    Time = {list_to_integer(H),list_to_integer(M),list_to_integer(S)},
    calendar:datetime_to_gregorian_seconds({Day,Time}). 


