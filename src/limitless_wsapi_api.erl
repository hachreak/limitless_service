%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_api).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  is_reached/1,
  setup/2
]).

%%%_ * API -------------------------------------------------------------

is_reached(ObjectIdString) ->
  {ok, LimitlessCtx} = limitless:init(),
  ObjectId = swagger_routerl_utils:to_binary(ObjectIdString),
  {IsReached, _, InfoObjects} = limitless:is_reached([ObjectId], LimitlessCtx),
  #{<<"is_reached">> => IsReached, <<"info">> => extract_info(InfoObjects)}.

setup(ObjectIdString, GroupIdString) ->
  {ok, LimitlessCtx} = limitless:init(),
  ObjectId = swagger_routerl_utils:to_binary(ObjectIdString),
  GroupId = swagger_routerl_utils:to_atom(GroupIdString),
  limitless:setup(ObjectId, GroupId, LimitlessCtx).

%% Private functions

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
