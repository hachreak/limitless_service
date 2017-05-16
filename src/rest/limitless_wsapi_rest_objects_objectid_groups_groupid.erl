%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_rest_objects_objectid_groups_groupid).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  from_json/2
]).

-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    resource_exists/2,
    rest_init/2
]).

%%%_ * API -------------------------------------------------------------

init(_Transport, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, AppCtx) ->
  {[<<"PUT">>], Req, AppCtx}.

rest_init(Req, AppCtx) ->
  {ObjectId, Req2} = cowboy_req:binding(objectid, Req),
  {GroupId, Req3} = cowboy_req:binding(groupid, Req2),
  {ok, Req3, AppCtx#{objectid => ObjectId,
                     groupid => swagger_routerl_utils:to_atom(GroupId)}}.

content_types_accepted(Req, AppCtx) ->
  {[{<<"application/json">>, from_json}], Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"application/json">>, to_json}], Req, AppCtx}.

resource_exists(ReqData, Context) ->
  case cowboy_req:method(ReqData) of
    <<"POST">> -> {false, ReqData, Context};
    _ -> {true, ReqData, Context}
  end.

from_json(Req, #{objectid := ObjectId, groupid := GroupId}=AppCtx) ->
  limitless_wsapi_api:setup(ObjectId, GroupId),
  {true, Req, AppCtx}.
