%%%-------------------------------------------------------------------
%% @doc limitless_service websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_ws_objects_objectid_groups_groupid).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId, GroupIdString], _AppCtx) ->
  limitless_service_api:setup(ObjectId, GroupIdString),
  {reply, #{}, Req}.
