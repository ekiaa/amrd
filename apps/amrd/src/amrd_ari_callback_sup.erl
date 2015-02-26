-module(amrd_ari_callback_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) when is_list(Args) ->
	supervisor:start_child(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [
		{amrd_ari_callback, {amrd_ari_callback, start_link, []}, temporary, 1000, worker, [amrd_ari_callback]}]}}.