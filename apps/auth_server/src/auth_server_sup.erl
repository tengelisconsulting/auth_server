%%%-------------------------------------------------------------------
%% @doc auth_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(auth_server_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(RuntimeArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [RuntimeArgs]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([RuntimeArgs]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 0,
                 period => 1},
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
    ChildSpecs = [
                  ReqMgrSup,
                  PgSup
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
