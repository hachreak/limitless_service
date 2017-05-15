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

put(_Event, Req, [ObjectId, GroupIdString], AppCtx) ->
  {ok, LimitlessCtx} = limitless:init(),
  GroupId = swagger_routerl_utils:to_atom(GroupIdString),
  limitless:setup(ObjectId, GroupId, LimitlessCtx),
  {reply, #{}, Req}.
