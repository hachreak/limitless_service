%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_ws_objects_objectid__isreached).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(_Event, Req, [ObjectId], _AppCtx) ->
  {ok, LimitlessCtx} = limitless:init(),
  {IsReached, _, InfoObjects} = limitless:is_reached([ObjectId], LimitlessCtx),
  Result = #{<<"is_reached">> => IsReached,
             <<"info">> => extract_info(InfoObjects)},
  {reply, Result, Req}.


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
