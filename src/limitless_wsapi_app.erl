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
  WsPort = application:get_env(limitless_wsapi, ws_port, 8080),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", limitless_wsapi_handler, {}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, WsPort}], [
    {env, [{dispatch, Dispatch}]}
  ]),

  limitless_wsapi_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
