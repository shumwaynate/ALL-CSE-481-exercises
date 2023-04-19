%%%-------------------------------------------------------------------
%% @doc ccbuilder public API
%% @end
%%%-------------------------------------------------------------------

-module(ccbuilder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ccbuilder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
