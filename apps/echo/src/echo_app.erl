%%%-------------------------------------------------------------------
%% @doc echo public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(SUPERVISOR, echo_sup).

start(_StartType, _StartArgs) ->
    case ?SUPERVISOR:start_link() of
        {ok, Pid} ->
            spawn(?SUPERVISOR, start_server, []),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%% internal functions
