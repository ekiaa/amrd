-module(amrd_ari_callback).

-behaviour(gen_server).

%% API functions
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start(VisNum, OpNums) ->
	amrd_ari_callback_sup:start_child([VisNum, OpNums]).

start_link(VisNum, OpNums) ->
	gen_server:start_link(?MODULE, {VisNum, OpNums}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({VisNum, OpNums}) ->
	ID = get_callback_id(),
	lager:debug("[~p] init process", [ID]),
	self() ! init,
	{ok, #{id => ID, opnums => OpNums, visnum => VisNum, hanguped => false}};

init(Args) ->
	lager:error("no matching: ~p", [Args]),
	{stop, {error, nomatch}}.

%%====================================================================

handle_call(Request, From, State) ->
	lager:error("no matching: ~p", [{From, Request}]),
	{stop, {error, nomatch}, {error, nomatch}, State}.

%%====================================================================

handle_cast(Message, State) ->
	lager:error("no matching: ~p", [Message]),
	{stop, {error, nomatch}, State}.

%%====================================================================

handle_info(init, #{id := ID} = State) ->
	{ok, Host} = application:get_env(ari_host),
	{ok, Port} = application:get_env(ari_port),
	{ok, Username} = application:get_env(ari_username),
	{ok, Secret} = application:get_env(ari_secret),
	lager:debug("[~p] try to start the ari: ~p", [ID, {Host, Port, Username, Secret}]),
	case amrd_ari_channel:start(self(), ID, Host, Port, Username, Secret) of
		{ok, ARI} ->
			lager:debug("[~p] ari has started: ~p", [ID, ARI]),
			erlang:monitor(process, ARI),
			self() ! init_operator_call,
			{noreply, State#{
				ari => ARI, 				% amrd_ari_channel pid
				opchid => undefined, 		% operator channel id
				vischid => undefined,		% visitor channel id
				bridge_id => undefined}};	% bridge id
		Reason ->
			lager:error("[~p] ari does not start: ~p", [ID, Reason]),
			{stop, {error, ari_not_started}, State}
	end;

handle_info(init_operator_call, #{id := ID, opnums := OpNums, ari := ARI} = State) ->
	FwdPeer = case application:get_env(fwdpeer) of {ok, Bin} when is_binary(Bin) -> to_list(<<Bin/binary,"/">>); _ -> [] end,
	Request = "/ari/channels?endpoint=SIP/" ++ FwdPeer ++ OpNums ++ "&app=" ++ ID ++ "&appArgs=operator",
	{ok, Ref} = amrd_ari_channel:request(ARI, Request, post),
	lager:debug("[~p] made request to ari for operator: ~p", [ID, Request]),
	{noreply, State#{ref_op => Ref}};

handle_info({'DOWN', _Ref, process, ARI, Reason}, #{id := ID, ari := ARI} = State) ->
	lager:debug("[~p] the ari is stopped: ~p", [ID, Reason]),
	{stop, Reason, State};

%%--------------------------------------------------------------------

handle_info({ari_event, #{<<"type">> := <<"ChannelVarset">>}}, State) ->
	{noreply, State};

handle_info({ari_event, #{<<"type">> := <<"StasisStart">>, <<"args">> := [<<"operator">>], <<"channel">> := #{<<"id">> := OpChID}}} = _Event, #{id := ID, visnum := VisNum, ari := ARI} = State) ->
	lager:debug("[~p] StasisStart for operator; OpChID: ~p", [ID, OpChID]),
	FwdPeer = case application:get_env(fwdpeer) of {ok, Bin} when is_binary(Bin) -> to_list(<<Bin/binary,"/">>); _ -> [] end,
	Request = "/ari/channels?endpoint=SIP/" ++ FwdPeer ++ VisNum ++ "&app=" ++ ID ++ "&appArgs=visitor",
	{ok, Ref} = amrd_ari_channel:request(ARI, Request, post),
	lager:debug("[~p] made request to ari for visitor: ~p", [ID, Request]),
	{noreply, State#{ref_vis => Ref}};

handle_info({ari_event, #{<<"type">> := <<"StasisStart">>, <<"args">> := [<<"visitor">>], <<"channel">> := #{<<"id">> := VisChID}}} = _Event, #{id := ID, ari := ARI} = State) ->
	lager:debug("[~p] StasisStart for visitor; VisChID: ~p", [ID, VisChID]),
	Request = "/ari/bridges",
	{ok, Ref} = amrd_ari_channel:request(ARI, Request, post),
	lager:debug("[~p] made request to ari for bridge: ~p", [ID, Request]),
	{noreply, State#{ref_br => Ref}};

handle_info({ari_event, #{<<"type">> := <<"ChannelDestroyed">>, <<"channel">> := #{<<"id">> := ChID}}}, #{bridge_id := BridgeID, opchid := OpChID, vischid := VisChID, id := ID, ari := ARI} = State) ->
	lager:debug("[~p] ChannelDestroyed: ~p", [ID, ChID]),
	BridgeID == undefined orelse amrd_ari_channel:request(ARI, to_list(<<"/ari/bridges/",BridgeID/binary>>), delete),
	case {OpChID, VisChID} of
		{ChID, _} when VisChID /= undefined ->
			amrd_ari_channel:request(ARI, to_list(<<"/ari/channels/",VisChID/binary>>), delete),
			{noreply, State#{opchid => undefined}};
		{_, ChID} when OpChID /= undefined ->
			amrd_ari_channel:request(ARI, to_list(<<"/ari/channels/",OpChID/binary>>), delete),
			{noreply, State#{vischid => undefined}};
		_ ->
			{stop, normal, State}
	end;

handle_info({ari_event, _} = _Event, #{id := _ID} = State) ->
	% lager:debug("[~p] no matching ari_event:~n~p", [_ID, _Event]),
	{noreply, State};

%%--------------------------------------------------------------------

handle_info({ari_response, Ref, #{<<"id">> := OpChID} = Response}, #{ref_op := Ref, id := ID} = State) ->
	lager:debug("[~p] operator channel id: ~p~nResponse^ ~p", [ID, OpChID, Response]),
	{noreply, State#{opchid => OpChID}};

handle_info({ari_response, Ref, #{<<"id">> := VisChID}}, #{ref_vis := Ref, id := ID} = State) ->
	lager:debug("[~p] visitor channel id: ~p", [ID, VisChID]),
	{noreply, State#{vischid => VisChID}};

handle_info({ari_response, Ref, #{<<"id">> := BridgeID}}, #{ref_br := Ref, opchid := OpChID, vischid := VisChID, id := ID, ari := ARI} = State) ->
	lager:debug("[~p] the bridge has created; BridgeID: ~p", [ID, BridgeID]),
	Request = to_list(<<"/ari/bridges/",BridgeID/binary,"/addChannel?channel=",OpChID/binary,",",VisChID/binary>>),
	amrd_ari:request(ARI, Request, post),
	lager:debug("[~p] made request to ari to add channels in the bridge: ~p", [ID, Request]),
	{noreply, State#{bridge_id => BridgeID}};

handle_info({ari_response, _, _} = _Response, #{id := _ID} = State) ->
	% lager:debug("[~p] no matching ari_response:~n~p", [_ID, _Response]),
	{noreply, State};

%%--------------------------------------------------------------------

handle_info(Info, State) ->
	lager:error("no matching: ~p", [Info]),
	{stop, {error, nomatch}, State}.

%%====================================================================

terminate(Reason, State) ->
	lager:debug("terminate; Reason: ~p; State: ~p", [Reason, State]), 
	ok.

%%====================================================================

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

get_callback_id() ->
	{_, A2, _} = erlang:now(),
	integer_to_list(A2).

to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin).