%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(auth_con_sup).

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
      auth_host:=Host,
      auth_port:=PortStr
     } = Args,
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    AuthCon = #{id => auth_con,
               start => {auth_con, 
                         start_link, 
                         [Host, list_to_integer(PortStr)]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [auth_con]},
    {ok, {SupFlags, [AuthCon]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
