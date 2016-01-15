%% @author igorzinin
%% @doc @todo Add description to evideomagic_sup.


-module(evideomagic_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
%% ====================================================================
init([]) ->
    MaxRestart = 6,
    MaxTime = 3000,
    SupFlags = {one_for_all, MaxRestart, MaxTime},
    ChildSpec = {evideomagic,
                {evideomagic, start_link, []},
                permanent,
                brutal_kill,
                worker,
                [evideomagic]},
    {ok,{SupFlags,[ChildSpec]}}.


