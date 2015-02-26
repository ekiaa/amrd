-module(amrd_ami_callback_sup).

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

start_child(Channel) ->
	supervisor:start_child(?MODULE, [Channel]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [
		{amrd_ami_callback, {amrd_ami_callback, start_link, []}, temporary, 1000, worker, [amrd_ami_callback]}]}}.