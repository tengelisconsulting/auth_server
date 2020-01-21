%%%-------------------------------------------------------------------
%% @doc auth_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(auth_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
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
    %% ReqSupChild = #{
    %%                id => req_sup_0,
    %%                start => {req_sup,
    %%                          start_link,
    %%                          [[
    %%                            #{id => pg,
    %%                              host => "127.0.0.1",
    %%                              port => 5000}
    %%                           ]]},
    %%                 restart => permanent,
    %%                 shutdown => 5000,
    %%                 type => supervisor,
    %%                 modules => [req_sup]
    %%               },
    ChildSpecs = [
                  ReqMgrSup
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
