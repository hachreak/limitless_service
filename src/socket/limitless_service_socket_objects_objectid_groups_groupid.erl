%%%-------------------------------------------------------------------
%% @doc limitless_service socket (websocket or tcp) handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_socket_objects_objectid_groups_groupid).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId, GroupIdString], AppCtx) ->
  limitless_service_api:setup(ObjectId, GroupIdString),
  {reply, #{}, Req, AppCtx}.
