%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(token_sup).

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
init([Args]) ->
    #{
      priv_key_file := PrivKeyFile
     } = Args,
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Token = #{id => token,
               start => {token, start_link, [PrivKeyFile]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [token]},
    {ok, {SupFlags, [Token]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
