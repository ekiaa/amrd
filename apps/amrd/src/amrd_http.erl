-module(amrd_http).

-include_lib("inets/include/httpd.hrl").

-export([do/1]).

do(ModData) -> 
	try
		% lager:debug("[do] ModData: ~p", [ModData]),
		Req = ModData#mod.request_uri,
		lager:debug("[do] request_uri: ~p", [Req]),
		case Res = parse(Req) of
			{ok, ["ami", "callback"], Params} ->
				gen_event:notify(amrd_events, {callback, ami, Params}),
				lager:debug("ami callback params: ~p", [Params]),
				{proceed, [{response, {200, "OK"}}]};
			{ok, ["ari", "callback"], Params} ->
				gen_event:notify(amrd_events, {callback, ari, Params}),
				lager:debug("ari callback params: ~p", [Params]),
				{proceed, [{response, {200, "OK"}}]};
			{ok, Path, Params} ->
				lager:debug("[do] parse request: ~p", [Res]),
				{proceed, [{response, {404, "Not Found"}}]};
			{error, Reason} ->
				lager:error("[do] parse request error: ~p", [Res]),
				{proceed, [{response, {400, "Bad Request"}}]}
		end
	catch
		Type:Error -> 
			lager:error("[do] error: ~p", [{Type, Error}]),
			{proceed, [{response, {500, "Internal Server Error"}}]}
	end.

parse(Req) ->
	case string:tokens(Req, "?") of
		[Path, Params] ->
			case parse_pair(lists:reverse(string:tokens(Params, "&"))) of
				{ok, Map} -> {ok, string:tokens(Path, "/"), Map};
				{error, Reason} -> {error, Reason}
			end;
		Res ->
			lager:error("[do] no matching request: ~p", [Res]),
			{error, nomatch}
	end.

parse_pair(List) ->
	parse_pair(List, []).
parse_pair([], KVList) ->
	{ok, maps:from_list(KVList)};
parse_pair([Pair | Rest], KVList) ->
	case string:tokens(Pair, "=") of
		[Key, Value] ->
			case string:tokens(Value, ",") of
				[Val | []] -> parse_pair(Rest, [{Key, Val} | KVList]);
				List -> parse_pair(Rest, [{Key, List} | KVList])
			end;
		Res ->
			lager:error("[do] no matching key-value pair: ~p", [Res]),
			{error, nomatch}
	end.
