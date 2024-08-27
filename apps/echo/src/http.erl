-module(http).

-export([get_mpv/1, get_header_lines/1]).

-spec get_mpv(Msg :: string()) -> {ok, string()}.
get_mpv(Msg) ->
    Lines = string:split(Msg, "\r\n", all),
    Result = lists:nth(1, Lines),
    {ok, Result}.

-spec get_header_lines(Header :: string()) -> {ok, [string()]}.
get_header_lines(Header) ->
    Headers = string:split(Header, "\r\n", all),
    Lines =
        [H || H <- Headers, string:is_empty(H) =:= false, length(string:split(H, ": ")) =:= 2],

    Result =
        lists:map(
            fun(Line) ->
                [Key, Value] = string:split(Line, ": "),
                {Key, Value}
            end,
            Lines
        ),

    {ok, Result}.
