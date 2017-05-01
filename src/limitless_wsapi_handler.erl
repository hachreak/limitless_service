%%%-------------------------------------------------------------------
%% @doc limitless_wsapi websocket handler
%% @end
%%%-------------------------------------------------------------------

-module(limitless_wsapi_handler).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_init/3,
  websocket_terminate/3
]).

init(_Transport, _Req, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, LimitlessCtx} = limitless:init(),
  {ok, Req, LimitlessCtx}.

websocket_handle({text, Msg}, Req, LimitlessCtx) ->
  Result = dispatch(jsx:decode(Msg, [return_maps]), LimitlessCtx),
  Resp = jsx:encode(Result),
  {reply, {text, Resp}, Req, LimitlessCtx};
websocket_handle(_Data, Req, LimitlessCtx) ->
    {ok, Req, LimitlessCtx}.

websocket_info(_Info, Req, LimitlessCtx) ->
  {ok, Req, LimitlessCtx}.

websocket_terminate(_Reason, _Req, _LimitlessCtx) ->
  ok.

%% Private functions

dispatch(#{<<"command">> := <<"setup">>,
           <<"context">> := #{<<"objectids">> := ObjectIds,
                              <<"group">> := StringGroup}}, LimitlessCtx) ->
  Group = to_atom(StringGroup),
  lists:map(fun(ObjectId) ->
      limitless:setup(ObjectId, Group, LimitlessCtx)
    end, ObjectIds);
dispatch(#{<<"command">> := <<"is_reached">>,
           <<"context">> := #{<<"objectids">> := ObjectIds}}, LimitlessCtx) ->
  {IsReached, ObjectId, InfoObjects} = limitless:is_reached(
                                         ObjectIds, LimitlessCtx),
  #{<<"is_reached">> => IsReached, <<"objectid">> => ObjectId,
    <<"info">> => extract_info(InfoObjects)}.

to_atom(Binary) when is_binary(Binary) -> to_atom(binary_to_list(Binary));
to_atom(String) when is_list(String) -> list_to_atom(String);
to_atom(Atom) -> Atom.

extract_info(InfoObjects) ->
  lists:map(fun({IsReached, ObjectId, ExtraInfo}) ->
      #{<<"is_reached">> => IsReached, <<"objectid">> => ObjectId,
        <<"extra">> => extract_extra_info(ExtraInfo)}
    end, InfoObjects).

extract_extra_info(ExtraInfo) ->
  lists:map(fun({Type, Max, Remaining, Expiry}) ->
       #{<<"type">> => Type, <<"max">> => Max,
         <<"remaining">> => Remaining, <<"expiry">> => Expiry}
    end, ExtraInfo).
