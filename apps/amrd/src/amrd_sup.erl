-module(amrd_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Module, Args) ->
	supervisor:start_child(?MODULE, {Module, {Module, start_link, Args}, permanent, 5000, worker, [Module]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {one_for_one, 5, 10}, [
		{amrd_ami_callback_sup, {amrd_ami_callback_sup, start_link, []}, permanent, infinity, supervisor, [amrd_ami_callback_sup]},
		{amrd_ari_callback_sup, {amrd_ari_callback_sup, start_link, []}, permanent, infinity, supervisor, [amrd_ari_callback_sup]},
		{amrd_ari_channel_sup, {amrd_ari_channel_sup, start_link, []}, permanent, infinity, supervisor, [amrd_ari_channel_sup]},
		{amrd_events, {amrd_events, start_link, []}, permanent, 5000, worker, dynamic}
	]}}.

