%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(req_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(ConnConfigs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, ConnConfigs).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init(ConnConfigs) ->
    Workers = [conf_to_worker(Conf) || Conf <- ConnConfigs],
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    {ok, {SupFlags, Workers}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
conf_to_worker(#{id:=Id, host:=Host, port:=Port}) ->
    #{id => Id,
      start => {requests, start_link, [Id, [Host, Port]]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [requests]}.
