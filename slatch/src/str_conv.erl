-module(str_conv).

-export([int_list_to_string/1, strip_game_id/1]).
-export([string_to_int/1, string_to_int_list/1]).

-export([split_message/1, determine_msg_type/3]).

%% Various helper functions for converting strings
%% to types and vice versa.

int_list_to_string([]) ->
    "[]";
int_list_to_string([Int | Rest]) when is_integer(Int) ->
    int_list_to_string(Rest, "[" ++ integer_to_list(Int)).

int_list_to_string([], Acc) ->
    Acc ++ "]";
int_list_to_string([Int | Rest], Acc) when is_integer(Int) ->
    int_list_to_string(Rest, Acc ++ "," ++ integer_to_list(Int)).

strip_game_id(Bin) when is_binary(Bin) ->
    Str = binary_to_list(Bin),
    case strip_to_colon(Str) of
        {[], _} -> none;
        {GameIDStr, Cmd} ->
            {GameID, []} = string:to_integer(GameIDStr),
            {GameID, Cmd}
    end.

strip_to_colon(Str) ->
    strip_to_colon(Str, "").

strip_to_colon([], Acc) ->
    {lists:reverse(Acc), []};
strip_to_colon([$: | Str], Acc) ->
    {Pre, []} = strip_to_colon([], Acc),
    {Pre, Str};
strip_to_colon([Char | Str], Acc) ->
    strip_to_colon(Str, [Char | Acc]).

string_to_int(Str) ->
    {Int, ""} = string:to_integer(Str),
    Int.

string_to_int_list(Str) ->
    string_to_int_list(Str, []).

string_to_int_list([$]], Acc) ->
    lists:reverse(Acc);
string_to_int_list([Char | Str], Acc)
    when Char == $, orelse Char == $[ ->
        {NxtInt, Rest} = string:to_integer(Str),
        string_to_int_list(Rest, [NxtInt | Acc]).

split_message(Msg) ->
    split_message(Msg, []).

split_message([], Acc) ->
    list_to_tuple(lists:reverse(Acc));
split_message([${ | Msg], Acc) ->
    {CurMsg, [$} | Rest]}
        = lists:splitwith(fun(Char) ->
                                  Char /= $}
                          end, Msg),
    split_message(Rest, [CurMsg | Acc]).


%% My most proud coding ever...
determine_msg_type(_P1Msg, _P2Msg, "d") ->
    display;
determine_msg_type([$i |_P1Msg], _P2Msg, _GameResourceMsg) ->
    target;
determine_msg_type(_P1Msg, [$i |_P2Msg], _GameResourceMsg) ->
    target;
determine_msg_type(_P1Msg, _P2Msg, [$i | _GameResourceMsg]) ->
    target;
determine_msg_type(_P1Msg, _P2Msg, [$r | _GameResourceMsg]) ->
    random;
determine_msg_type([$[ |_P1Msg], _P2Msg, _GameResourceMsg) ->
    order;
determine_msg_type(_P1Msg, [$[ |_P2Msg], _GameResourceMsg) ->
    order;
determine_msg_type(_P1Msg, _P2Msg, [$[ | _GameResourceMsg]) ->
    order;
determine_msg_type("config", "config", "") ->
    start.
