-module(utils).

-export([
    slice_binary/3,
    time/0
]).

-spec number_to_weekday(integer()) -> string().
number_to_weekday(1) ->
    "Mon";
number_to_weekday(2) ->
    "Tue";
number_to_weekday(3) ->
    "Wed";
number_to_weekday(4) ->
    "Thu";
number_to_weekday(5) ->
    "Fri";
number_to_weekday(6) ->
    "Sat";
number_to_weekday(7) ->
    "Sun".

-spec number_to_month(integer()) -> string().
number_to_month(1) ->
    "Jan";
number_to_month(2) ->
    "Feb";
number_to_month(3) ->
    "Mar";
number_to_month(4) ->
    "Apr";
number_to_month(5) ->
    "May";
number_to_month(6) ->
    "Jun";
number_to_month(7) ->
    "Jul";
number_to_month(8) ->
    "Aug";
number_to_month(9) ->
    "Sep";
number_to_month(10) ->
    "Oct";
number_to_month(11) ->
    "Nov";
number_to_month(12) ->
    "Dec".

-spec slice_binary(binary(), non_neg_integer(), non_neg_integer()) -> {ok, binary()}.
slice_binary(Bin, Start, Len) ->
    case Len > byte_size(Bin) - Start of
        true ->
            <<_:Start/binary, Rest/binary>> = Bin,
            {ok, Rest};
        false ->
            <<_:Start/binary, Rest/binary>> = Bin,
            <<Slice:Len/binary, _/binary>> = Rest,
            {ok, Slice}
    end.

-spec time() -> {ok, string()}.
time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Week = calendar:day_of_the_week({Year, Month, Day}),

    H = string:pad(integer_to_list(Hour), 2, leading, $0),
    M = string:pad(integer_to_list(Minute), 2, leading, $0),
    S = string:pad(integer_to_list(Second), 2, leading, $0),

    {ok,
        number_to_weekday(Week) ++ " " ++ number_to_month(Month) ++ " " ++ integer_to_list(Day) ++
            " " ++ H ++ ":" ++ M ++ ":" ++ S}.
