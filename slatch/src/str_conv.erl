-module(str_conv).

-export([int_list_to_string/1, strip_game_id/1]).
-export([string_to_int/1, string_to_int_list/1]).

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
