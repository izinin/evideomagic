%% @author igorzinin
%% @doc @todo Add description to evideomagic_app.


-module(evideomagic_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    evideomagic_sup:start_link().

stop(_State) ->
    ok.
