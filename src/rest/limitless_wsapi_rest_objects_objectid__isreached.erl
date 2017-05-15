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
  {ok, LimitlessCtx} = limitless:init(),
  {ObjectId, Req2} = cowboy_req:binding(objectid, Req),
  {ok, Req2, AppCtx#{limitless => LimitlessCtx, objectid => ObjectId}}.

content_types_accepted(Req, AppCtx) ->
  {[{<<"application/json">>, from_json}], Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"application/json">>, to_json}], Req, AppCtx}.

from_json(Req, #{limitless := LimitlessCtx, objectid := ObjectId}=AppCtx) ->
  {IsReached, _, InfoObjects} = limitless:is_reached([ObjectId], LimitlessCtx),
  Result = #{<<"is_reached">> => IsReached,
             <<"info">> => extract_info(InfoObjects)},
  Json = jsx:encode(Result),
  Req2 = cowboy_req:set_resp_body(Json, Req),
  {true, Req2, AppCtx}.

%%% Private functions

extract_info(InfoObjects) ->
  lists:map(fun({IsReached, _, ExtraInfo}) ->
      #{<<"is_reached">> => IsReached,
        <<"extra">> => extract_extra_info(ExtraInfo)}
    end, InfoObjects).

extract_extra_info(ExtraInfo) ->
  lists:map(fun({Type, Max, Remaining, Expiry}) ->
       #{<<"group">> => Type, <<"max">> => Max,
         <<"remaining">> => Remaining, <<"expiry">> => Expiry}
    end, ExtraInfo).
