%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(req_mgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ReqMgr = #{id => req_mgr,
               start => {req_mgr, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [req_mgr]},
    {ok, {SupFlags, [ReqMgr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
