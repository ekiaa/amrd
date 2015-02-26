-module(amrd_events).

-behaviour(gen_event).

%% API functions

-export([start_link/0]).

%% gen_event callbacks

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	lager:debug("start"),
	Result = gen_event:start_link({local, ?MODULE}),
	gen_event:add_handler(?MODULE, ?MODULE, "AMI events handler"),
	Result.

%%====================================================================
%% gen_event callbacks
%%====================================================================

init(Args) ->
	lager:debug("init; Args: ~p", [Args]),
	ets:new(callbacks, [named_table, public]),
	self() ! init,
	{ok, #{}}.

%%====================================================================

handle_event({ami_event, _, #{<<"Event">> := <<"Newchannel">>, <<"Channel">> := Channel}} = Event, State) -> 
	amrd_ami_callback:start(Channel),
	amrd_ami_callback:event(Channel, Event),
	{ok, State};

handle_event({ami_event, _, #{<<"Channel">> := Channel}} = Event, State) ->
	amrd_ami_callback:event(Channel, Event),
	{ok, State};

handle_event({ami_event, _, #{<<"Channel1">> := Channel1, <<"Channel2">> := Channel2}} = Event, State) ->
	amrd_ami_callback:event(Channel1, Event),
	amrd_ami_callback:event(Channel2, Event),
	{ok, State};

handle_event({ami_event, _, _} = _Event, State) ->
	% lager:debug("no matching ami_event:~n~p", [_Event]),
	{ok, State};

%%--------------------------------------------------------------------

handle_event({callback, ami, Params}, #{ami := AMI} = State) ->
	lager:debug("ami callback params: ~p", [Params]),
	CallbackID = list_to_binary(get_callback_id()),
	ets:insert(callbacks, {CallbackID, Params#{id => CallbackID}}),
	Res = do_callback_by_ami(AMI, CallbackID),
	lager:debug("do_callback_by_ami return: ~p", [Res]),
	{ok, State};
	
handle_event({callback, ari, Params}, State) ->
	lager:debug("ari callback params: ~p", [Params]),
	#{"operator" := OpNum, "visitor" := VisNum} = Params,
	Res = amrd_ari_callback:start(VisNum, OpNum),
	lager:debug("start callback: ~p", [Res]),
	{ok, State};

handle_event(Event, State) ->
	lager:debug("no matching Event:~n~p", [Event]),
	{ok, State}.

%%====================================================================

handle_call(Request, _State) ->
	lager:error("handle_call: no matching Request: ~p", [Request]),
	{remove_handler, {error, nomatch}}.

%%====================================================================

handle_info(init, State) ->
	{ok, Host} = application:get_env(ami_host),
	{ok, Port} = application:get_env(ami_port),
	{ok, Username} = application:get_env(ami_username),
	{ok, Secret} = application:get_env(ami_secret),
	case ami:create(?MODULE, Host, Port, Username, Secret) of
		{ok, Pid} ->
			{ok, State#{interface => ami, ami => Pid}};
		Other ->
			lager:error("ami:create() return: ~p", [Other]),
			remove_handler
	end;

handle_info({ami_reply, From, Reply}, State) ->
	lager:debug("ami_event from ~p:~n~p", [From, Reply]),
	{ok, State};

handle_info(Info, _State) ->
	lager:error("handle_info: nomatch Info: ~p", [Info]),
	remove_handler.

%%====================================================================

terminate(_Args, _State) ->
	ok.

%%====================================================================

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_callback_by_ami(AMI, CallbackID) ->
	Cmd = #{
		<<"Action">>      => <<"Originate">>, 
		<<"Channel">>     => <<"Local/",CallbackID/binary,"@callback/n">>, 
		<<"Application">> => <<"AGI">>,
		<<"Data">>        => <<"agi:async">>,
		<<"Async">>       => <<"true">>},
	ami:send(AMI, Cmd).

get_callback_id() ->
	{_, A2, _} = erlang:now(),
	integer_to_list(A2).