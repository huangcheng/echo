-module(html).

-define(ELEMENT_PER_LINE, 16).

-export([build/3]).

-spec build(tuple(), non_neg_integer(), binary()) -> {ok, string()}.
build(Ip, Port, Packet) ->
    {ok, Head} = head(),
    {ok, Remote} = remote(Ip, Port),
    {ok, Method} = method(binary_to_list(Packet)),
    {ok, Headers} = headers(binary_to_list(Packet)),
    {ok, Raw} = raw(Packet),
    {ok, Time} = get_time(),
    {ok, Body} = body(Remote, Method, Headers, Raw, Time),
    {ok, Content} = content(Head, Body),

    packet(Content).

-spec head() -> {ok, string()}.
head() ->
    Head =
        "<head><style>body {color: black; background-color: white;} "
        "tr:hover { background: yellow }</style></head>\n",
    {ok, Head}.

-spec remote(tuple(), integer()) -> {ok, string()}.
remote(Ip, Port) ->
    {First, Second, Third, Fourth} = Ip,
    Address =
        integer_to_list(First) ++
            "." ++
            integer_to_list(Second) ++
            "." ++
            integer_to_list(Third) ++
            "." ++
            integer_to_list(Fourth),
    Remote = "<h2>Remote: " ++ Address ++ " " ++ integer_to_list(Port) ++ "</h2>\n",
    {ok, Remote}.

-spec method(string()) -> {ok, string()}.
method(Packet) ->
    case http:get_mpv(Packet) of
        {ok, Method} ->
            HTML = "<h2>" ++ Method ++ "</h2>\n",
            {ok, HTML}
    end.

-spec headers(string()) -> {ok, string()}.
headers(Packet) ->
    {ok, Headers} = http:get_header_lines(Packet),

    HTML =
        "<table mini:hint=\"folded;Headers\" border=\"0\" cellpadding=\"3\" "
        "cellspacing=\"0\">\n" ++
            lists:foldl(
                fun({Key, Value}, Acc) ->
                    Acc ++
                        "<tr><td valign=\"top\">" ++
                        Key ++
                        "</td><td>" ++
                        Value ++
                        "</td></tr>\n"
                end,
                "",
                Headers
            ) ++
            "</table><hr noshade=\"\">\n",

    {ok, HTML}.

-spec body(string(), string(), string(), string(), string()) -> {ok, string()}.
body(Remote, Method, Headers, Raw, Time) ->
    Result = "<body>" ++ Method ++ Remote ++ Headers ++ Raw ++ Time ++ "</body>\n",

    {ok, Result}.

-spec content(string(), string()) -> {ok, string()}.
content(Head, Body) ->
    Content = "<html>\n" ++ Head ++ Body ++ "</html>\n",

    {ok, Content}.

-spec raw(Packet :: binary()) -> {ok, Content :: string()}.
raw(Packet) ->
    Len = byte_size(Packet),
    Count = Len div ?ELEMENT_PER_LINE,
    Remind = Len rem ?ELEMENT_PER_LINE,

    Lines =
        case Remind of
            0 ->
                Count;
            _ ->
                Count + 1
        end,

    Raw = get_raw(Packet, 0, Lines),

    Result = [L || L <- Raw, length(L) > 0],

    Content =
        "<div mini:hint=\"folded;Raw Request\"><h2>Raw request</h2><pre>" ++
            lists:foldl(fun(Line, Acc) -> Acc ++ Line end, "", Result) ++
            "</pre></div><hr noshade=\"\">\n",

    {ok, Content}.

-spec get_raw(binary(), integer(), integer()) -> [string()].
get_raw(Packet, Index, Lines) when Index < Lines ->
    Start = Index * ?ELEMENT_PER_LINE,
    {ok, Binary} = utils:slice_binary(Packet, Start, ?ELEMENT_PER_LINE),
    Raw = binary_to_list(Binary),
%%    Raw = string:slice(binary_to_list(Packet), Start, ?ELEMENT_PER_LINE),

    LineNo = io_lib:format("~3.16. B", [Index * ?ELEMENT_PER_LINE]),

    Hex = lists:foldl(fun(E, Acc) -> Acc ++ io_lib:format(" ~2.16.0B", [E]) end, "", Raw),

    HexLen = 3 * ?ELEMENT_PER_LINE,

    String =
        lists:foldl(
            fun(E, Acc) ->
                case E of
                    E when E >= 32, E =< 126 -> Acc ++ [E];
                    _ -> Acc ++ "."
                end
            end,
            "",
            Raw
        ),

    Line = LineNo ++ ":" ++ string:pad(Hex, HexLen, trailing, " ") ++ "   " ++ String ++ "\n",

    [Line | get_raw(Packet, Index + 1, Lines)];
get_raw(_, Index, Lines) when Index == Lines ->
    [].

-spec get_time() -> {ok, string()}.
get_time() ->
    {ok, Time} = utils:time(),
    Content = "<p>Page generated at " ++ Time ++ "</p>\n",

    {ok, Content}.

-spec packet(Content :: string()) -> {ok, string()}.
packet(Content) ->
    Packet =
        "HTTP/1.1 200 OK\r\n" ++
            "Content-Type: text/html\r\n" ++
            "Content-Length: " ++
            integer_to_list(byte_size(list_to_bitstring(Content))) ++
            "\r\n" ++
            "\r\n" ++
            Content,

    {ok, Packet}.
