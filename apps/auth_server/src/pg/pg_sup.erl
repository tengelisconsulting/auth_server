%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(pg_sup).

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
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

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
init([Args]) ->
    #{
      db_api_host:=Host,
      db_api_port:=Port
     } = Args,
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Pg = #{id => pg,
               start => {pg, start_link, [Host, Port]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [pg]},

    {ok, {SupFlags, [Pg]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
