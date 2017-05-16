%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_ws_objects_objectid_groups_groupid).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId, GroupIdString], _AppCtx) ->
  limitless_wsapi_api:setup(ObjectId, GroupIdString),
  {reply, #{}, Req}.
