%%%-------------------------------------------------------------------
%% @doc teambuilder public API
%% @end
%%%-------------------------------------------------------------------

-module(teambuilder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    teambuilder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
