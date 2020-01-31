%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(api_server_sup).

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
%% -spec start_link() -> {ok, Pid :: pid()} |
%%                       {error, {already_started, Pid :: pid()}} |
%%                       {error, {shutdown, term()}} |
%%                       {error, term()} |
%%                       ignore.
start_link(RuntimeArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [RuntimeArgs]).

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
init([RuntimeArgs]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 0,
                 period => 5},
    ReqMgrSup = #{
                  id => req_mgr_sup,
                  start => {req_mgr_sup, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [req_mgr_sup]
                 },
    PgSup = #{
              id => pg_sup,
              start => {pg_sup, start_link, [RuntimeArgs]},
              restart => permanent,
              shutdown => 5000,
              type => supervisor,
              modules => [pg_sup]
             },
    ProxySup = #{id => proxy_sup,
                 start => {proxy_sup, start_link, []},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [proxy_sup]},
    {ok, {SupFlags, [
                     ReqMgrSup,
                     PgSup,
                     ProxySup
                    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
