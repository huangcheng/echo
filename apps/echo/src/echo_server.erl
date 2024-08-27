-module(echo_server).

-define(APPLICATION, echo).

-export([
    start/0
]).

-export([
    server/1,
    listen/0
]).

-spec start() -> {ok, pid()}.
start() ->
    Pid = spawn_link(?MODULE, listen, []),
    register(?MODULE, Pid),
    {ok, Pid}.

-spec listen() -> ok | {error, term()}.
listen() ->
    PORT = application:get_env(?APPLICATION, port, 8080),
    WORKERS = application:get_env(?APPLICATION, workers, 30),

    process_flag(trap_exit, true),

    case gen_tcp:listen(PORT, [{active, false}, binary]) of
        {ok, ListenSock} ->
            start_servers(WORKERS, ListenSock),
            monitor(ListenSock);
        {error, Reason} ->
            {error, Reason}
    end.

monitor(LS) ->
    receive
        {'EXIT', _Pid, _Reason} ->
            start_servers(1, LS),
            monitor(LS);
        _ ->
            ok
    end.

start_servers(0, _ListenSock) ->
    ok;
start_servers(N, ListenSock) ->
    spawn_link(?MODULE, server, [ListenSock]),
    start_servers(N - 1, ListenSock).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok, S} ->
            loop(S),
            server(LS);
        _ ->
            ok
    end.

loop(S) ->
    inet:setopts(S, [{active, once}]),
    receive
        {tcp, S, Data} ->
            {ok, {Ip, Port}} = inet:peername(S),
            {ok, Packet} = html:build(Ip, Port, Data),
            gen_tcp:send(S, [Packet]),
            loop(S);
        {tcp_closed, S} ->
            ok
    end.
