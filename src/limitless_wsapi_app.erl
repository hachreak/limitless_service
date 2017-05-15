%%%-------------------------------------------------------------------
%% @doc limitless_wsapi application
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-type appctx() :: any().
-type key()      :: binary().
-type pairs()  :: list({key(), value()}).
-type value()    :: binary().

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  WsPort = application:get_env(limitless_wsapi, ws_port, 8080),
  Protocol = application:get_env(limitless_wsapi, protocol, http),
  Dispatch = cowboy_router:compile([
    {'_', routes(#{protocol => Protocol}) ++ [
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

-spec routes(appctx()) -> {http | https, cowboy_router:routes()}.
routes(#{protocol := Protocol}=AppCtx) ->
  Filename = swagger_filename(),
  Yaml = swagger_routerl:load(Filename),
  {ok, SwaggerFileRaw} = file:read_file(Filename),

  FileEndpoint = swagger_routerl_cowboy_rest:file_endpoint(
    SwaggerFileRaw, #{endpoint => endpoint(Yaml),
    protocol => swagger_routerl_utils:to_binary(Protocol)}),
  WSEndpoint = swagger_routerl_cowboy_ws:compile(
    "limitless_wsapi_ws_", Yaml, fuubar, #{
    handler => swagger_routerl_cowboy_v1_ws_json_dispatcher
  }),

  FileEndpoint ++ WSEndpoint.

swagger_filename() ->
  PrivDir = code:priv_dir(limitless_wsapi),
  Filename = "docs/swagger.yaml",
  filename:join([PrivDir, Filename]).

endpoint(Yaml) ->
  Version = swagger_routerl:get_version(Yaml),
  "/" ++ Version ++ "/docs/swagger.yaml".
