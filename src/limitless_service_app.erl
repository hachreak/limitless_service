%%%-------------------------------------------------------------------
%% @doc limitless_service application
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-type appctx() :: any().

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ServicesConfig = application:get_env(limitless_service, services, []),

  lists:foreach(
    fun(http) ->
      HttpConfig = application:get_env(limitless_service, http, []),
      HttpPort = proplists:get_value(port, HttpConfig, 8080),
      Protocol = proplists:get_value(protocol, HttpConfig, http),
      http(HttpPort, Protocol);
       (tcp) ->
      TcpConfig = application:get_env(limitless_service, tcp, []),
      TcpServerName = proplists:get_value(name, TcpConfig, "limitless"),
      TcpPort = proplists:get_value(port, TcpConfig, 54355),
      tcp(TcpServerName, TcpPort);
       (Service) ->
      error_logger:warning_msg("Unknown ~p service", [Service])
    end, ServicesConfig),

  limitless_service_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

tcp(TcpServerName, TcpPort) ->
  Filename = swagger_filename(),
  Yaml = swagger_routerl:load(Filename),
  {Name, Port, Handler, AppCtx} = swagger_routerl_tcp:compile(
    "limitless_service_socket_", Yaml, fuubar, #{
      name => TcpServerName, port => TcpPort
    }),

  {ok, _} = ranch:start_listener(
    Name, 100, ranch_tcp, [{port, Port}], Handler, AppCtx).

http(HttpPort, Protocol) ->
  Dispatch = cowboy_router:compile([
    {'_', routes(#{protocol => Protocol})}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, HttpPort}], [
    {env, [{dispatch, Dispatch}]}
  ]).

-spec routes(appctx()) -> {http | https, cowboy_router:routes()}.
routes(#{protocol := Protocol}) ->
  Filename = swagger_filename(),
  Yaml = swagger_routerl:load(Filename),
  {ok, SwaggerFileRaw} = file:read_file(Filename),

  FileEndpoint = swagger_routerl_cowboy_rest:file_endpoint(
    SwaggerFileRaw, #{endpoint => endpoint(Yaml),
    protocol => swagger_routerl_utils:to_binary(Protocol)}),
  RestEndpoints = swagger_routerl_cowboy_rest:compile(
    "limitless_service_rest_", Yaml, #{}),
  WSEndpoint = swagger_routerl_cowboy_ws:compile(
    "limitless_service_socket_", Yaml, fuubar, #{
    handler => swagger_routerl_cowboy_v1_ws_json_dispatcher
  }),

  FileEndpoint ++ RestEndpoints ++ WSEndpoint.

swagger_filename() ->
  PrivDir = code:priv_dir(limitless_service),
  Filename = "docs/swagger.yaml",
  filename:join([PrivDir, Filename]).

endpoint(Yaml) ->
  Version = swagger_routerl:get_version(Yaml),
  "/" ++ Version ++ "/docs/swagger.yaml".
