-module(amrd_ari_channel).

-behaviour(websocket_client_handler).

%% API functions
-export([start/6, start_link/6, request/3]).

%% websocket_client_handler callbacks
-export([init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%%====================================================================
%% API functions
%%====================================================================

start(Owner, App, Host, Port, Username, Secret) ->
	amrd_ari_channel_sup:start_child([Owner, App, Host, Port, Username, Secret]).

start_link(Owner, App, Host, Port, Username, Secret) ->
	Request = lists:flatten(io_lib:format("ws://~s:~w/ari/events?api_key=~s:~s&app=~s", [Host, Port, Username, Secret, App])),
	lager:debug("try to start the websocket_client~nRequest: ~p", [Request]),
	websocket_client:start_link(Request, ?MODULE, {Owner, App, Host, Port, Username, Secret}).

request(Pid, Request, Method) ->
	Ref = erlang:make_ref(),
	Pid ! {request, Request, Method, Ref},
	{ok, Ref}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------

init({Owner, App, Host, Port, Username, Secret}, _ConnState) ->
	lager:debug("init ari; Owner: ~p; Application: ~p ", [Owner, App]),
	erlang:monitor(process, Owner),
	{ok, #{owner => Owner, app => App, host => Host, port => Port, username => Username, secret => Secret}}.

%%--------------------------------------------------------------------

websocket_handle({text, Text}, _ConnState, #{owner := Owner} = State) ->
	Owner ! {ari_event, json_to_map(jsonx:decode(Text, [{format, struct}]))},
	{ok, State};

websocket_handle(Message, _ConnState, State) ->
	lager:debug("no matching~nMessage: ~p", [Message]),
	{ok, State}.

%%--------------------------------------------------------------------

websocket_info({'DOWN', _Ref, process, Owner, Reason}, _ConnState, #{app := App, owner := Owner} = State) ->
	lager:debug("[~p] the owner is stopped: ~p", [App, Reason]),
	{close, <<>>, State};

websocket_info({request, Request, Method, Ref}, _ConnState, #{owner := Owner, host := Host, port := Port, username := Username, secret := Secret} = State) ->
	Req = lists:flatten(io_lib:format("http://~s:~w~s", [Host, Port, Request])),
	case ibrowse:send_req(Req, [], Method, [], [{basic_auth, {Username, Secret}}]) of
		{ok, "200", _Headers, Body} ->
			Owner ! {ari_response, Ref, json_to_map(jsonx:decode(list_to_binary(Body), [{format, struct}]))},
			{ok, State};
		Result ->
			Owner ! {ari_response, Ref, Result},
			{ok, State}
	end;

websocket_info(Info, _ConnState, State) ->
	lager:debug("no matching~nInfo: ~p", [Info]),
	{ok, State}.

%%--------------------------------------------------------------------

websocket_terminate(Reason, _ConnState, State) -> 
	lager:debug("websocket_terminate; Reason: ~p; State: ~p", [Reason, State]), 
	ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

json_to_map({struct, List}) when is_list(List) -> 
	json_to_map(List, #{});
json_to_map(Value) -> 
	Value.
json_to_map([], Map) ->
	Map;
json_to_map([{Key, Value} | Rest], Map) ->
	json_to_map(Rest, maps:put(Key, json_to_map(Value), Map)).