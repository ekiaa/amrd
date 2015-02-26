-module(amrd_ami_callback).

-behaviour(gen_server).

%% API functions
-export([start/1, start_link/1, event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start(Channel) ->
	amrd_ami_callback_sup:start_child(Channel).

start_link(Channel) ->
	gen_server:start_link({via, global, Channel}, ?MODULE, {channel, Channel}, []).

event(Channel, Event) ->
	% lager:debug("[~p] event:~n~p", [Channel, Event]),
	case global:whereis_name(Channel) of
		undefined -> lager:error("[~p] not found process; Event: ~p", [Channel, Event]);
		Pid -> gen_server:cast(Pid, Event)
	end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------

init({channel, Channel}) ->
	lager:debug("[~p] init process", [Channel]),
	{ok, #{channel => Channel}};

init(Args) ->
	lager:error("no matching: ~p", [Args]),
	{stop, {error, nomatch}}.

%%--------------------------------------------------------------------

handle_call(Request, From, State) ->
	lager:error("no matching: ~p", [{From, Request}]),
	{stop, {error, nomatch}, {error, nomatch}, State}.

%%--------------------------------------------------------------------

handle_cast({ami_event, _, #{<<"Event">> := <<"Newchannel">>, <<"Context">> := <<"callback">>, <<"Exten">> := CallbackID}}, #{channel := <<"Local",_/binary>> = Channel} = State) ->
	[{CallbackID, #{"operator" := OpNum, "visitor" := VisNum}}] = ets:lookup(callbacks, CallbackID),
	case lists:reverse(binary_to_list(Channel)) of
		[$1 | _] ->
			lager:debug("[~p] channel for visitor; CallbackID: ~p", [Channel, CallbackID]),
			{noreply, State#{peer => visitor, callback_id => CallbackID, number => list_to_binary(VisNum)}};
		[$2 | _] ->
			lager:debug("[~p] channel for operator; CallbackID: ~p", [Channel, CallbackID]),
			{noreply, State#{peer => operator, callback_id => CallbackID, number => list_to_binary(OpNum)}}
	end;

handle_cast({ami_event, AMI, #{<<"Event">> := <<"AsyncAGIStart">>}}, #{peer := operator, number := Number, channel := Channel} = State) ->
	FwdPeer = case application:get_env(fwdpeer) of {ok, Bin} when is_binary(Bin) -> <<Bin/binary,"/">>; _ -> <<>> end,
	Res = ami:send(AMI, #{<<"Action">> => <<"AGI">>, <<"Channel">> => Channel, <<"Command">> => <<"EXEC Dial SIP/", FwdPeer/binary, Number/binary, ",15">>, <<"CommandID">> => <<"Dial">>}),
	lager:debug("[~p] dial to operator: ~p", [Channel, Res]),
	{noreply, State};

handle_cast({ami_event, AMI, #{<<"Event">> := <<"AsyncAGIStart">>}}, #{peer := visitor, number := Number, channel := Channel} = State) ->
	FwdPeer = case application:get_env(fwdpeer) of {ok, Bin} when is_binary(Bin) -> <<Bin/binary,"/">>; _ -> <<>> end,
	Res = ami:send(AMI, #{<<"Action">> => <<"AGI">>, <<"Channel">> => Channel, <<"Command">> => <<"EXEC Dial SIP/", FwdPeer/binary, Number/binary, ",15">>, <<"CommandID">> => <<"Dial">>}),
	lager:debug("[~p] dial to visitor: ~p", [Channel, Res]),
	{noreply, State};

handle_cast({ami_event, AMI, #{<<"Event">> := <<"AsyncAGIExec">>, <<"CommandId">> := <<"Dial">>}}, #{channel := Channel} = State) ->
	ami:send(AMI, #{<<"Action">> => <<"AGI">>, <<"Channel">> => Channel, <<"Command">> => <<"ASYNCAGI BREAK">>, <<"CommandID">> => <<"HangupChannel">>}),
	{noreply, State};

handle_cast({ami_event, _, #{<<"Event">> := <<"Hangup">>, <<"Cause">> := Cause, <<"Cause-txt">> := CauseTxt}}, #{peer := operator, callback_id := CallbackID, channel := Channel} = State) ->
	lager:debug("[~p] ami event Hangup: ~p ~p", [Channel, Cause, CauseTxt]),
	true = ets:delete(callbacks, CallbackID),
	{stop, normal, State};

handle_cast({ami_event, _, #{<<"Event">> := <<"Hangup">>, <<"Cause">> := Cause, <<"Cause-txt">> := CauseTxt}}, #{channel := Channel} = State) ->
	lager:debug("[~p] ami event Hangup: ~p ~p", [Channel, Cause, CauseTxt]),
	{stop, normal, State};

handle_cast({ami_event, _, _Event}, State) ->
	% lager:debug("[~p] no matching ami_event:~n~p", [_Event]),
	{noreply, State};

handle_cast(Message, State) ->
	lager:error("no matching: ~p", [Message]),
	{stop, {error, nomatch}, State}.

%%--------------------------------------------------------------------

handle_info(Info, State) ->
	lager:error("no matching: ~p", [Info]),
	{stop, {error, nomatch}, State}.

%%--------------------------------------------------------------------

terminate(Reason, State) -> 
	lager:debug("terminate: Reason: ~p; State: ~p", [Reason, State]), 
	ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------