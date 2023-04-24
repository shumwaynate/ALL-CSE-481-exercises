%%%-------------------------------------------------------------------
%% @doc friendstorage public API
%% @end
%%%-------------------------------------------------------------------

-module(friendstorage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    friendstorage_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
