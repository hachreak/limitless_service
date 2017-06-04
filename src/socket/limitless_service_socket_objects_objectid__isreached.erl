%%%-------------------------------------------------------------------
%% @doc limitless_service socket (websocket or tcp) handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_socket_objects_objectid__isreached).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId], AppCtx) ->
  Result = limitless_service_api:is_reached(ObjectId),
  {reply, Result, Req, AppCtx}.
