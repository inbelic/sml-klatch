-module(misc).

%% Various common helper functions and types

-export([int_list_to_string/1, string_to_int_list/1]).
-export([split_to_messages/1]).
-export([strip_game_id/1, strip_to_colon/1, bin_rev/1]).

%% Common types
-type game_id() :: integer().

-export_type([game_id/0]).

-spec int_list_to_string([integer()]) -> string().
int_list_to_string([]) ->
    "[]";
int_list_to_string([Int | Rest]) when is_integer(Int) ->
    int_list_to_string(Rest, "[" ++ integer_to_list(Int)).

int_list_to_string([], Acc) ->
    Acc ++ "]";
int_list_to_string([Int | Rest], Acc) when is_integer(Int) ->
    int_list_to_string(Rest, Acc ++ "," ++ integer_to_list(Int)).

-spec string_to_int_list(string()) -> [integer()].
string_to_int_list(Str) ->
    string_to_int_list(Str, []).

string_to_int_list([$]], Acc) ->
    lists:reverse(Acc);
string_to_int_list([Char | Str], Acc)
    when Char == $, orelse Char == $[ ->
        {NxtInt, Rest} = string:to_integer(Str),
        string_to_int_list(Rest, [NxtInt | Acc]).

-spec strip_game_id(binary()) -> {game_id(), binary()}.
strip_game_id(Bin) when is_binary(Bin) ->
    case strip_to_colon(Bin) of
        {[], _} -> none;
        {GameIDStr, Cmd} ->
            {GameID, []} = string:to_integer(GameIDStr),
            {GameID, Cmd}
    end.

-spec strip_to_colon(binary()) -> {binary(), binary()}.
strip_to_colon(Bin) ->
    strip_to_char($:, Bin, <<>>).

strip_to_char(Char, Bin) ->
    strip_to_char(Char, Bin, <<>>).

strip_to_char(_Char, <<>>, Acc) ->
    {bin_rev(Acc), <<>>};
strip_to_char(Char, [Char | Rest], Acc) ->
    {Pre, <<>>} = strip_to_char(Char, <<>>, Acc),
    {Pre, Rest};
strip_to_char(Char, [H | Rest], Acc) ->
    strip_to_char(Char, Rest, [H | Acc]).

-spec split_to_messages(binary()) -> [binary()].
split_to_messages(Bin) ->
    split_to_messages(Bin, []).

split_to_messages(<<>>, Acc) ->
    lists:reverse(Acc);
split_to_messages(Bin, Acc) ->
    {CurMsg, Rest} = split_message(Bin),
    split_to_messages(Rest, [CurMsg | Acc]).

split_message(<<${, Bin/binary>>) ->
    strip_to_char($}, Bin).

-spec bin_rev(binary()) -> binary().
bin_rev(Bin) ->
    bin_rev(Bin, <<>>).

bin_rev(<<>>, Acc) ->
    Acc;
bin_rev(<<H:1/binary, Rest/binary>>, Acc) ->
    bin_rev(Rest, <<H/binary, Acc/binary>>).
