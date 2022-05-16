%%%-------------------------------------------------------------------
%% @doc cbeam public API
%% @end
%%%-------------------------------------------------------------------

-module(cbeam_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cbeam_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
