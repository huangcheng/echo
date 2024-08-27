-module(echo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([start_server/0]).

-define(SERVER, ?MODULE).

-define(CHILD_MODULE, echo_server).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(Args :: list()) ->
    {
        ok,
        {
            SupFlags :: #{strategy := atom()},
            ChildSpecs :: [
                #{
                    id := term(),
                    start := {module(), atom(), [term()]},
                    restart := atom(),
                    shutdown := atom(),
                    type := worker,
                    modules := [module()]
                }
            ]
        }
    }.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},

    ChildSpecs = [
        #{
            id => ?CHILD_MODULE,
            start => {?CHILD_MODULE, start, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [?CHILD_MODULE]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

start_server() ->
    supervisor:start_child(?MODULE, []).
