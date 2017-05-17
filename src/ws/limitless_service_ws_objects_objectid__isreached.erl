%%%-------------------------------------------------------------------
%% @doc limitless_service websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_ws_objects_objectid__isreached).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId], _AppCtx) ->
  Result = limitless_service_api:is_reached(ObjectId),
  {reply, Result, Req}.
