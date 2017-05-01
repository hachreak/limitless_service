%%%-------------------------------------------------------------------
%% @doc limitless_wsapi application
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", limitless_wsapi_handler, {}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),

  limitless_wsapi_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
