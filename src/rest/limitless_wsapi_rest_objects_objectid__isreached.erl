%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_rest_objects_objectid__isreached).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  from_json/2
]).

-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    rest_init/2
]).

%%%_ * API -------------------------------------------------------------

init(_Transport, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, AppCtx) ->
  {[<<"PUT">>], Req, AppCtx}.

rest_init(Req, AppCtx) ->
  {ObjectId, Req2} = cowboy_req:binding(objectid, Req),
  {ok, Req2, AppCtx#{objectid => ObjectId}}.

content_types_accepted(Req, AppCtx) ->
  {[{<<"application/json">>, from_json}], Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"application/json">>, to_json}], Req, AppCtx}.

from_json(Req, #{objectid := ObjectId}=AppCtx) ->
  Result = limitless_wsapi_api:is_reached(ObjectId),
  Req2 = cowboy_req:set_resp_body(jsx:encode(Result), Req),
  {true, Req2, AppCtx}.
